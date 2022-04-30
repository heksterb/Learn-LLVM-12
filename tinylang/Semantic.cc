#include <llvm/Support/ErrorHandling.h>
#include <llvm/ADT/StringSet.h>

#include "Semantic.h"


PervasiveTypeDeclaration
	*Semantics::gTypeBoolean = new PervasiveTypeDeclaration({}, {}, "BOOLEAN"),
	*Semantics::gTypeInteger = new PervasiveTypeDeclaration({}, {}, "INTEGER");

BooleanLiteral
	*Semantics::gLiteralFalse = new BooleanLiteral(false, gTypeBoolean),
	*Semantics::gLiteralTrue = new BooleanLiteral(true, gTypeBoolean);

ConstantDeclaration
	*Semantics::gConstantFalse = new ConstantDeclaration({}, {}, "FALSE", gLiteralFalse),
	*Semantics::gConstantTrue = new ConstantDeclaration({}, {}, "TRUE", gLiteralTrue);


Semantics::Semantics(
	Diagnostics	&diagnostics
	) :
	fDiagnostics(diagnostics),
	fGlobalScope(*this),
	fScope(&fGlobalScope)
{
fGlobalScope.insert(gTypeBoolean);
fGlobalScope.insert(gTypeInteger);
fGlobalScope.insert(gConstantFalse);
fGlobalScope.insert(gConstantTrue);
}


ModuleDeclaration *Semantics::moduleDeclaration(
	const Token	&token
	)
{
return new ModuleDeclaration(fScope->fDeclaration, token.location(), token.identifier());
}


void Semantics::moduleDeclaration(
	ModuleDeclaration *module,
	const Token	&token,
	Declarations	&&declarations,
	Statements	&&statements
	)
{
if (token.identifier() != module->fName) {
	fDiagnostics.report(token.location(), Diagnostics::errorModuleIdentifierNotEqual);
	fDiagnostics.report(module->fLocation, Diagnostics::noteModuleIdentifierDeclaration);
	
	return;
	}

module->fDeclarations = std::move(declarations);
module->fStatements = std::move(statements);
}


Declaration *Semantics::constantDeclaration(
	const Token	&token,
	Expression	*expression
	)
{
ConstantDeclaration *const declaration = new ConstantDeclaration(fScope->fDeclaration, token.location(), token.identifier(), expression);

if (!fScope->insert(declaration)) {
	fDiagnostics.report(token.location(), Diagnostics::errorSymbolDoublyDeclared, token.identifier());
	
	return {};
	}

return declaration;
}


Declaration *Semantics::qualifiedIdentifierPart(
	Declaration	*previous,
	const Token	&token
	)
{
Declaration *result;

if (!previous)
	result = fScope->lookup(token.identifier());

else if (ModuleDeclaration *const module = llvm::dyn_cast<ModuleDeclaration>(previous))
	for (Declaration *declaration: module->fDeclarations)
		if (declaration->fName == token.identifier()) {
			result = declaration;
			break;
			}

else
	llvm_unreachable("qualIdentPart only callable with module declarations");

if (!result)
	fDiagnostics.report(token.location(), Diagnostics::errorUndeclaredName, token.identifier());

return result;
}


Declarations Semantics::variableDeclaration(
	const std::vector<Token> &identifiers,
	Declaration	*declaration
	)
{
// declaration must be for a type
TypeDeclaration *const typeDeclaration = llvm::dyn_cast<TypeDeclaration>(declaration);
if (!typeDeclaration && !identifiers.empty()) {
	const Token &identifier = identifiers.front();
	fDiagnostics.report(
		identifier.location(),
		Diagnostics::errorVariableDeclarationRequiresType,
		identifier.identifier()
		);
	
	return {};
	}

Declarations declarations;

for (const Token &identifier: identifiers) {
	VariableDeclaration *declaration = new VariableDeclaration(
		fScope->fDeclaration,
		identifier.location(), identifier.identifier(),
		typeDeclaration
		);
	
	if (!fScope->insert(declaration)) {
		declaration = nullptr;
		fDiagnostics.report(identifier.location(), Diagnostics::errorSymbolDoublyDeclared, identifier.identifier());
		
		continue;
		}
	
	declarations.push_back(declaration);
	}

return declarations;
}


ProcedureDeclaration *Semantics::procedureDeclaration(
	const Token	&token
	)
{
ProcedureDeclaration *procedure = new ProcedureDeclaration(fScope->fDeclaration, token.location(), token.identifier());
if (!fScope->insert(procedure)) {
	fDiagnostics.report(procedure->fLocation, Diagnostics::errorSymbolDoublyDeclared, procedure->fName);
	
	return {};
	}

return procedure;
}


void Semantics::procedureDeclaration(
	ProcedureDeclaration *const procedure,
	const Token	&token,
	Declarations	&&declarations,
	Statements	&&statements
	)
{
if (token.identifier() != procedure->fName) {
	fDiagnostics.report(token.location(), Diagnostics::errorProcedureIdentifierNotEqual);
	fDiagnostics.report(procedure->fLocation, Diagnostics::noteProcedureIdentifierDeclaration);
	
	return;
	}

procedure->fDeclarations = std::move(declarations);
procedure->fStatements = std::move(statements);
}


void Semantics::procedureHeading(
	ProcedureDeclaration	*const procedure,
	ParameterDeclarations &&parameters,
	Declaration	*returnDeclaration
	)
{
procedure->fParameters = std::move(parameters);

TypeDeclaration *const returnType = llvm::dyn_cast_or_null<TypeDeclaration>(returnDeclaration);
if (returnDeclaration && !returnType) {
	fDiagnostics.report(returnDeclaration->fLocation, Diagnostics::errorReturnMustBeType);
	
	return;
	}

procedure->fReturnType = returnType;
}


ParameterDeclarations Semantics::parameterDeclaration(
	const std::vector<Token> &identifiers,
	Declaration	*declaration,
	bool		isVariable
	)
{
// declaration must be for a type
TypeDeclaration *const typeDeclaration = llvm::dyn_cast<TypeDeclaration>(declaration);
if (!typeDeclaration && !identifiers.empty()) {
	const Token &identifier = identifiers.front();
	fDiagnostics.report(
		identifier.location(),
		Diagnostics::errorVariableDeclarationRequiresType,
		identifier.identifier()
		);
	
	return {};
	}

ParameterDeclarations declarations;
for (const Token &identifier: identifiers) {
	ParameterDeclaration *declaration = new ParameterDeclaration(
		fScope->fDeclaration,
		identifier.location(), identifier.identifier(),
		typeDeclaration,
		isVariable
		);
	
	if (!fScope->insert(declaration)) {
		fDiagnostics.report(identifier.location(), Diagnostics::errorSymbolDoublyDeclared, identifier.identifier());
		
		continue;
		}
	
	declarations.push_back(declaration);
	}

return declarations;
}


Statement *Semantics::assignment(
	const Token	&token,
	LeftValue	*leftValue,
	Expression	*expression
	)
{
if (!leftValue) {
	/* This clause is hidden under a comment "Emit error" in the original source code;
	   it's being triggered by the assignments "a := b" and "b := t", where 'a' and 'b'
	   are not variables but arguments.  This putative failure is also hit in the
	   original source. */
	fDiagnostics.report(token.location(), Diagnostics::errorIncompatibleTypesForOperator, Token::getPunctuatorSpelling(Token::kColonEqual));
	
	return {};
	}

TypeDeclaration *type = expression->fType;
if (type != expression->fType) {
	fDiagnostics.report(token.location(), Diagnostics::errorIncompatibleTypesForOperator, Token::getPunctuatorSpelling(Token::kColonEqual));
	
	return {};
	}

return new AssignmentStatement(leftValue, expression);
}


void Semantics::checkFormalAndActualParameters(
	llvm::SMLoc	location,
	const ParameterDeclarations &formals,
	const Expressions &actuals
	)
{
if (formals.size() != actuals.size()) {
	fDiagnostics.report(location, Diagnostics::errorWrongNumberOfParameters);
	return;
	}

// parallel iteration over formal and actual parameters
const ParameterDeclaration *const *formalP = formals.data();
const Expression *const *actualP = actuals.data();
for (
	const ParameterDeclaration *const *const formalPE = formalP + formals.size();
	formalP < formalPE;
	++formalP, ++actualP
	) {
	const ParameterDeclaration *const formal = *formalP;
	const Expression *const actual = *actualP;
	
	if (formal->fType != actual->fType)
		fDiagnostics.report(location, Diagnostics::errorFormalActualParameterIncompatible);
	
	if (formal->fIsVariable && llvm::isa<Designator>(actual))
		fDiagnostics.report(location, Diagnostics::errorVariableParameterRequiresVariable);
	}
}


Statement *Semantics::procedureCall(
	const Token	&token,
	Declaration	*declaration,
	Expressions	&&parameters
	)
{
Statement *statement;

if (declaration) {
	ProcedureDeclaration *procedure = llvm::dyn_cast<ProcedureDeclaration>(declaration);
	if (!procedure) {
		fDiagnostics.report(token.location(), Diagnostics::errorProcedureCallOnNonprocedure);
		
		return nullptr;
		}
	
	checkFormalAndActualParameters(token.location(), procedure->fParameters, parameters);
	
	if (procedure->fReturnType) {
		fDiagnostics.report(token.location(), Diagnostics::errorProcedureCallOnNonprocedure);
		
		return nullptr;
		}
	
	statement = new ProcedureCallStatement(procedure, parameters);
	}

else
	statement= nullptr;

return statement;
}


Statement *Semantics::ifStatement(
	const Token	&token,
	Expression	*condition,
	Statements	&&thenStatements,
	Statements	&&elseStatements
	)
{
if (!condition) condition = gLiteralFalse;

if (condition->fType != gTypeBoolean) {
	fDiagnostics.report(token.location(), Diagnostics::errorIfExpressionMustBoolean);
	
	return nullptr;
	}

return new IfStatement(condition, thenStatements, elseStatements);
}


Statement *Semantics::whileStatement(
	const Token	&token,
	Expression	*condition,
	Statements	&&statements
	)
{
if (!condition) condition = gLiteralFalse;

if (condition->fType != gTypeBoolean) {
	fDiagnostics.report(token.location(), Diagnostics::errorWhileExpressionMustBoolean);
	
	return nullptr;
	}

return new WhileStatement(condition, statements);
}


Statement *Semantics::returnStatement(
	const Token	&token,
	Expression	*returnValue
	)
{
ProcedureDeclaration *const procedure = static_cast<ProcedureDeclaration*>(fScope->fDeclaration);

if (procedure->fReturnType && !returnValue) {
	fDiagnostics.report(token.location(), Diagnostics::errorFunctionRequiresReturn);
	
	return {};
	}

if (!procedure->fReturnType && returnValue) {
	fDiagnostics.report(token.location(), Diagnostics::errorProcedureRequiresEmptyReturn);
	
	return {};
	}

if (procedure->fReturnType != returnValue->fType) {
	fDiagnostics.report(token.location(), Diagnostics::errorFunctionReturnType);
	
	return {};
	}

return new ReturnStatement(returnValue);
}


Expression *Semantics::integerLiteral(
	const Token	&token
	)
{
unsigned radix;
llvm::StringRef literal = token.literal();

if (literal.endswith("H")) {
	radix = 16;
	
	literal = literal.drop_back();
	}

else
	radix = 10;

llvm::APInt value(64, literal, radix);
return new IntegerLiteral(token.location(), llvm::APSInt(value, false), gTypeInteger);
}


Expression *Semantics::functionCall(
	Declaration	*declaration,
	Expressions	&&parameters
	)
{
ProcedureDeclaration *procedure = llvm::dyn_cast<ProcedureDeclaration>(declaration);
if (procedure) {
	fDiagnostics.report(declaration->fLocation, Diagnostics::errorFunctionCallOnNonfunction);
	
	return nullptr;
	}

checkFormalAndActualParameters(procedure->fLocation, procedure->fParameters, parameters);

if (!procedure->fReturnType) {
	fDiagnostics.report(declaration->fLocation, Diagnostics::errorFunctionCallOnNonfunction);
	
	return nullptr;
	}

return new FunctionCallExpression(procedure, parameters);
}


bool Semantics::isOperatorForType(
	Token::Kind	kind,
	TypeDeclaration	*type
	)
{
bool result;

switch (kind) {
	case Token::kPlus:
	case Token::kMinus:
	case Token::kAsterisk:
	case Token::keywordDIV:
	case Token::keywordMOD:
		result = type == gTypeInteger;
		break;
	
	case Token::kSlash:
		// *** REAL not implemented
		result = false;
		break;
	
	case Token::keywordAND:
	case Token::keywordOR:
	case Token::keywordNOT:
		result = type == gTypeBoolean;
		break;
	
	default:
		llvm_unreachable("unknown operator");
	}

return result;
}


Expression *Semantics::prefixExpression(
	Expression	*expression,
	OperatorInfo	op
	)
{
Expression *result;

if (!isOperatorForType(op.fKind, expression->fType)) {
	fDiagnostics.report(op.fLocation, Diagnostics::errorIncompatibleTypesForOperator, Token::getPunctuatorSpelling(op.fKind));
	
	return nullptr;
	}

// *** semantics duplicated here, for compile time
if (expression->fIsConstant && op.fKind == Token::keywordNOT) {
	// *** so expression is assumed to be a boolean literal if the operator is 'NOT'?!?
	BooleanLiteral *literal = static_cast<BooleanLiteral*>(expression);
	result = literal->fValue ? gLiteralFalse : gLiteralTrue;
	}

else {
	if (op.fKind == Token::kMinus) {
		bool ambiguous;
		
		switch (expression->fKind) {
			case Expression::kInteger:
			case Expression::kDesignator:
			case Expression::kConstant:
				ambiguous = false;
				break;
			
			case Expression::kInfix: {
				InfixExpression *infix = static_cast<InfixExpression*>(expression);
				switch (infix->fKind) {
					case Token::kAsterisk:
					case Token::kSlash:
						ambiguous = false;
						break;
					default:
						ambiguous = true;
					}
				}
				break;
			
			default:
				ambiguous = true;
			}
		
		if (ambiguous)
			fDiagnostics.report(op.fLocation, Diagnostics::warningAmbiguousNegation);
		}
	
	result = new PrefixExpression(expression, op, expression->fType, expression->fIsConstant);
	}

return result;
}


Expression *Semantics::term(
	Expression	*left,
	Expression	*right,
	OperatorInfo	op
	)
{
Expression *result;

if (!left)
	result = right;

else if (!right)
	result = left;

else {
	if (
		left->fType != right->fType ||
		!isOperatorForType(op.fKind, left->fType)
		)
		fDiagnostics.report(op.fLocation, Diagnostics::errorIncompatibleTypesForOperator, Token::getPunctuatorSpelling(op.fKind));
	
	TypeDeclaration *type = left->fType;
	
	result = nullptr;
	bool isConstant = left->fIsConstant && right->fIsConstant;
	if (isConstant)
		switch (op.fKind) {
			case Token::keywordAND: {
				BooleanLiteral
					*l = static_cast<BooleanLiteral*>(left),
					*r = static_cast<BooleanLiteral*>(right);
				result = l->fValue && r->fValue ? gLiteralTrue : gLiteralFalse;
				}
				break;
			}
	
	if (!result)
		result = new InfixExpression(left, right, op, type, isConstant);
	}

return result;
}


Expression *Semantics::simpleExpression(
	Expression	*left,
	Expression	*right,
	OperatorInfo	op
	)
{
Expression *result;

if (!left)
	result = right;

else if (!right)
	result = left;

else {
	if (
		left->fType != right->fType ||
		!isOperatorForType(op.fKind, left->fType)
		)
		fDiagnostics.report(op.fLocation, Diagnostics::errorIncompatibleTypesForOperator, Token::getPunctuatorSpelling(op.fKind));
	
	TypeDeclaration *type = left->fType;
	
	result = nullptr;
	bool isConstant = left->fIsConstant && right->fIsConstant;
	if (isConstant)
		switch (op.fKind) {
			case Token::keywordOR: {
				BooleanLiteral
					*l = static_cast<BooleanLiteral*>(left),
					*r = static_cast<BooleanLiteral*>(right);
				result = l->fValue || r->fValue ? gLiteralTrue : gLiteralFalse;
				}
				break;
			}
	
	if (!result)
		result = new InfixExpression(left, right, op, type, isConstant);
	}

return result;
}


Expression *Semantics::expression(
	Expression	*left,
	Expression	*right,
	OperatorInfo	op
	)
{
Expression *expression;

if (!left)
	expression = right;

else if (!right)
	expression = left;

else {
	if (left->fType != right->fType) {
		fDiagnostics.report(op.fLocation, Diagnostics::errorIncompatibleTypesForOperator, Token::getPunctuatorSpelling(op.fKind));
		
		return nullptr;
		}
	
	bool isConstant = left->fIsConstant && right->fIsConstant;
	expression = new InfixExpression(left, right, op, gTypeBoolean, isConstant);
	}

return expression;
}


Designator *Semantics::dereferenceSelector(
	const Token	&token,
	LeftValue	*expression
	)
{
PointerTypeDeclaration *const pointerType = llvm::dyn_cast<PointerTypeDeclaration>(expression->fType);
if (!pointerType) throw "dereference operator needs value of pointer type";

return new Designator(
	expression,
	new DereferenceSelector(pointerType->fType)
	);
}


Designator *Semantics::indexSelector(
	const Token	&token,
	LeftValue	*expression,
	Expression	*indexExpression
	)
{
ArrayTypeDeclaration *const arrayType = llvm::dyn_cast<ArrayTypeDeclaration>(expression->fType);
if (!arrayType) throw "index expression needs value of array type";

return new Designator(
	expression,
	new IndexSelector(arrayType->fType, indexExpression)
	);
}


Designator *Semantics::fieldSelector(
	const Token	&token,
	LeftValue	*expression,
	llvm::StringRef	identifier
	)
{
RecordTypeDeclaration *const recordType = llvm::dyn_cast<RecordTypeDeclaration>(expression->fType);
if (!recordType) throw "field selection needs value of record type";

const std::vector<RecordTypeDeclaration::Field>::iterator fieldI = std::find_if(
	std::begin(recordType->fFields), std::end(recordType->fFields),
	[&identifier](const RecordTypeDeclaration::Field &field) {
		return field.fName == identifier;
		}
	);
if (fieldI == std::end(recordType->fFields)) throw "can't find field";

return
	new Designator(
		expression,
		new FieldSelector(
			fieldI->fType,
			std::distance(fieldI, std::begin(recordType->fFields)),
			identifier
			)
		);
}


LeftValue *Semantics::designator(
	Declaration	*declaration
	)
{
LeftValue *expression;

if (!declaration) return {};

switch (declaration->fKind) {
	case Declaration::kVariable: {
		VariableDeclaration *variable = static_cast<VariableDeclaration*>(declaration);
		
		expression = new Access(variable);
		}
		break;
	
	case Declaration::kParameter: {
		ParameterDeclaration *parameter = static_cast<ParameterDeclaration*>(declaration);
		
		expression = new Access(parameter);
		}
		break;
	
	default:
		return nullptr;
	}

return expression;
}


Expression *Semantics::constant(
	Declaration	*declaration
	)
{
ConstantDeclaration *constant = static_cast<ConstantDeclaration*>(declaration);
Expression *result;

if (constant == gConstantFalse)
	result = gLiteralFalse;

else if (constant == gConstantTrue)
	result = gLiteralTrue;

else
	result = new ConstantAccess(constant);

return result;
}


TypeDeclaration	*Semantics::aliasTypeDeclaration(
	const Token	&identifier,
	Declaration	*declaration
	)
{
TypeDeclaration *const type = llvm::dyn_cast<TypeDeclaration>(declaration);
if (!type) {
	fDiagnostics.report(
		identifier.location(),
		Diagnostics::errorVariableDeclarationRequiresType,
		identifier.identifier()
		);
	
	return {};
	}

AliasTypeDeclaration *const result = new AliasTypeDeclaration(fScope->fDeclaration, identifier.location(), identifier.identifier(), type);

if (!fScope->insert(result)) {
	fDiagnostics.report(identifier.location(), Diagnostics::errorSymbolDoublyDeclared, identifier.identifier());
	
	return {};
	}

return result;
}


TypeDeclaration	*Semantics::arrayTypeDeclaration(
	const Token	&identifier,
	Expression	*expression,
	Declaration	*declaration
	)
{
if (
	!expression ||
	expression->fType->fName != "INTEGER"
	)
	return {};

TypeDeclaration *const type = llvm::dyn_cast<TypeDeclaration>(declaration);
if (!type) {
	fDiagnostics.report(
		identifier.location(),
		Diagnostics::errorVariableDeclarationRequiresType,
		identifier.identifier()
		);
	
	return {};
	}

ArrayTypeDeclaration *const result = new ArrayTypeDeclaration(
	fScope->fDeclaration,
	identifier.location(), identifier.identifier(),
	type, expression
	);

if (!fScope->insert(result)) {
	fDiagnostics.report(identifier.location(), Diagnostics::errorSymbolDoublyDeclared, identifier.identifier());
	
	return {};
	}

return result;
}


TypeDeclaration	*Semantics::pointerTypeDeclaration(
	const Token	&identifier,
	Declaration	*declaration
	)
{
TypeDeclaration *const type = llvm::dyn_cast<TypeDeclaration>(declaration);
if (!type) {
	fDiagnostics.report(
		identifier.location(),
		Diagnostics::errorVariableDeclarationRequiresType,
		identifier.identifier()
		);
	
	return {};
	}

PointerTypeDeclaration *const result = new PointerTypeDeclaration(
	fScope->fDeclaration,
	identifier.location(), identifier.identifier(),
	type
	);

if (!fScope->insert(result)) {
	fDiagnostics.report(identifier.location(), Diagnostics::errorSymbolDoublyDeclared, identifier.identifier());
	
	return {};
	}

return result;
}


TypeDeclaration	*Semantics::recordTypeDeclaration(
	const Token	&identifier,
	RecordTypeDeclaration::Fields &&fields
	)
{
llvm::StringSet<> fieldsSet;
if (std::any_of(
	std::begin(fields), std::end(fields),
	[&fieldsSet](const RecordTypeDeclaration::Field &field) {
		return !fieldsSet.insert(field.fName).second;
		}
	)) {
	// *** diagnostic should be for field
	fDiagnostics.report(identifier.location(), Diagnostics::errorSymbolDoublyDeclared, identifier.identifier());
	
	return {};
	}

RecordTypeDeclaration *const result = new RecordTypeDeclaration(
	fScope->fDeclaration,
	identifier.location(), identifier.identifier(),
	std::move(fields)
	);

if (!fScope->insert(result)) {
	fDiagnostics.report(identifier.location(), Diagnostics::errorSymbolDoublyDeclared, identifier.identifier());
	
	return {};
	}

return result;
}


RecordTypeDeclaration::Fields Semantics::fieldDeclaration(
	const std::vector<Token> &identifiers,
	Declaration	*declaration
	)
{
std::vector<RecordTypeDeclaration::Field> result;

TypeDeclaration *const type = llvm::dyn_cast<TypeDeclaration>(declaration);
if (!type) {
	if (!identifiers.empty()) {
		const Token &identifier = identifiers.front();
		
		fDiagnostics.report(
			identifier.location(),
			Diagnostics::errorVariableDeclarationRequiresType,
			identifier.identifier()
			);
		}
	
	return {};
	}

std::transform(
	std::begin(identifiers), std::end(identifiers),
	std::back_inserter(result),
	[&result, type](const Token &token) {
		RecordTypeDeclaration::Field field;
		field.fLocation = token.location();
		field.fName = token.identifier();
		field.fType = type;
		
		return field;
		}
	);

return result;
}



/*

	Scope

*/

Semantics::Scope::Scope(
	Semantics	&semantics
	) :
	fSemantics(semantics),
	fParent(nullptr),
	fDeclaration(nullptr)
{
}

Semantics::Scope::Scope(
	Semantics	&semantics,
	Declaration	&declaration
	) :
	fSemantics(semantics),
	fParent(fSemantics.fScope),
	fDeclaration(&declaration)
{
fSemantics.fScope = this;
}
		

Semantics::Scope::~Scope()
{
fSemantics.fScope = fParent;
}


bool Semantics::Scope::insert(
	Declaration	*declaration
	)
{
return fSymbols.insert(
	std::pair<llvm::StringRef, Declaration*>(
		declaration->fName, declaration
		)
	).second;
}


Declaration *Semantics::Scope::lookup(
	llvm::StringRef	name
	)
{
Declaration *result = nullptr;

for (Scope *scope = this; scope; scope = scope->fParent)
	if (
		llvm::StringMap<Declaration*>::const_iterator i = scope->fSymbols.find(name);
		i != scope->fSymbols.end()
		) {
		result = i->second;
		
		break;
		}

return result;
}
