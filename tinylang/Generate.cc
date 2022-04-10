#include <llvm/IR/Attributes.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/CFG.h>

#include "Generate.h"



/*

	Block

*/

/*	addEmptyPhi
	Add an empty 'phi' instruction for the declaration at the beginning of the block
*/
llvm::PHINode *Generator::Module::Procedure::Block::addEmptyPhi(
	const Declaration &declaration
	)
{
llvm::Type *const type = fProcedure.fGenerator.mapType(declaration);

return
	fBlock->empty() ?
		llvm::PHINode::Create(type, 0, "", fBlock) :
		llvm::PHINode::Create(type, 0, "", &fBlock->front());
}


/*	optimizePhi

*/
void Generator::Module::Procedure::Block::optimizePhi(
	llvm::PHINode	*phi
	)
{
llvm::Value *same = nullptr;
for (llvm::Value *value: phi->incoming_values())
	if (value == same || value == phi)
		// *** under what circumstances can the incoming value of the phi be the phi itself?!
		;
	
	else if (same && value != same)
		return;
	
	else
		same = value;

if (!same)
	same = llvm::UndefValue::get(phi->getType());

// collect phi instructions using this one
llvm::SmallVector<llvm::PHINode*, 8> candidatePHIs;
for (llvm::Use &use: phi->uses())
	if (llvm::PHINode *p = llvm::dyn_cast<llvm::PHINode>(use.getUser()))
		if (p != phi)
			candidatePHIs.push_back(p);

phi->replaceAllUsesWith(same);
phi->eraseFromParent();

for (llvm::PHINode *p: candidatePHIs)
	optimizePhi(p);
}


/*	addPhiOperands
	Add predecessor operands to the given incomplete phi node
*/
void Generator::Module::Procedure::Block::addPhiOperands(
	const Declaration &declaration,
	llvm::PHINode	*phi
	)
{
// for all predecessors to this block
for (llvm::BasicBlock *predecessor: llvm::predecessors(fBlock)) {
	Block &predecessorBlock = *fProcedure.BlockOf(predecessor);
	
	// add the value of the variable in that predecessor
	phi->addIncoming(
		predecessorBlock.read(declaration),
		predecessor
		);
	}

optimizePhi(phi);
}


/*	seal
	Mark that all our predecessors are now known
*/
void Generator::Module::Procedure::Block::seal()
{
assert(!fSealed);

// complete all the yet-incomplete PHI nodes
for (const std::pair<llvm::PHINode*, const Declaration*> &phi: fIncompletePhis)
	addPhiOperands(*phi.second, phi.first);
fIncompletePhis.clear();

/* Note that sealing doesn't mean we can either destruct the Block or remove it from
   the procedure; it's still referred to from addPhiOperands(). */
fSealed = true;
}


/*	read
	Return the value corresponding to the given declaration
*/
llvm::Value *Generator::Module::Procedure::Block::read(
	const Declaration &declaration
	)
{
llvm::Value *result;

assert(
	llvm::isa<VariableDeclaration>(declaration) ||
	llvm::isa<FormalParameterDeclaration>(declaration)
	);

// found definition of that declaration in this block?
if (
	llvm::DenseMap<const Declaration*, llvm::TrackingVH<llvm::Value>>::iterator declarationI = fDefinitions.find(&declaration);
	declarationI != fDefinitions.end()
	)
	result = declarationI->second;

// must be in a predecessor block
else {
	// don't know all predecessors yet?
	if (!fSealed) {
		// add incomplete phi as placeholder for variable
		llvm::PHINode *const phi = addEmptyPhi(declaration);
		fIncompletePhis[phi] = &declaration;
		
		result = phi;
		}
	
	// only one predecessor?
	else if (Block *predecessor = fProcedure.BlockOf(fBlock->getSinglePredecessor()))
		result = predecessor->read(declaration);
	
	// multiple prececessors
	else {
		// create empty phi instruction to break potential cycles
		llvm::PHINode *const phi = addEmptyPhi(declaration);
		addPhiOperands(declaration, phi);
		
		result = phi;
		}
	
	// add definition within the context of this block
	fDefinitions[&declaration] = result;
	}

return result;
}


/*	write
	Set the value corresponding to the given declaration
*/
void Generator::Module::Procedure::Block::write(
	const Declaration &declaration,
	llvm::Value	*value
	)
{
assert(
	llvm::isa<VariableDeclaration>(declaration) ||
	llvm::isa<FormalParameterDeclaration>(declaration)
	);

fDefinitions[&declaration] = value;
}



/*

	module procedure generator

*/

/*	createFunctionType
	Create the LLVM function type for the given procedure
	
	This is a static function because it is called during construction,
	when the instance doesn't yet exist.
*/
llvm::FunctionType *Generator::Module::Procedure::createFunctionType(
	Generator	&generator,
	const ProcedureDeclaration &declaration
	)
{
llvm::Type *const resultType =
	declaration.fReturnType ?
		generator.mapType(*declaration.fReturnType) :
		generator.fTypeVoid;

llvm::SmallVector<llvm::Type*, 8> parameterTypes;
const FormalParameterDeclarations &parameters = declaration.fParameters;
std::transform(
	std::begin(parameters), std::end(parameters),
	std::back_inserter(parameterTypes),
	[&generator](FormalParameterDeclaration *parameter) { return generator.mapType(*parameter); }
	);

return llvm::FunctionType::get(resultType, parameterTypes, false /* isVarArgs */);
}


/*	createFunction
	Create the LLVM function
	
	This is a static function because it is called during construction,
	when the instance doesn't yet exist.
*/
llvm::Function *Generator::Module::Procedure::createFunction(
	Generator	&generator,
	const ProcedureDeclaration &declaration,
	llvm::Module	&result
	)
{
llvm::FunctionType *functionType = createFunctionType(generator, declaration);
llvm::Function *function = llvm::Function::Create(
	functionType,
	llvm::GlobalValue::ExternalLinkage,
	generator.mangleName(declaration),
	result
	);

// give parameters a name
for (unsigned argumentI = 0; argumentI < function->arg_size(); argumentI++) {
	llvm::Argument &argument = *function->getArg(argumentI);
	FormalParameterDeclaration &parameterDeclaration = *declaration.fParameters[argumentI];
	
	if (parameterDeclaration.fIsVariable) {
		llvm::AttrBuilder attributes(generator.fContext);
		llvm::TypeSize size = result.getDataLayout().getTypeStoreSize(
			generator.mapType(*parameterDeclaration.fType)
			);
		attributes.addDereferenceableAttr(size);
		attributes.addAttribute(llvm::Attribute::NoCapture);
		argument.addAttrs(attributes);
		}
	
	argument.setName(parameterDeclaration.fName);
	}

return function;
}


/*	Procedure
	Prepare to emit code for a procedure
*/
Generator::Module::Procedure::Procedure(
	Module		&module,
	const ProcedureDeclaration &declaration,
	llvm::Module	&result
	) :
	fModule(module),
	fGenerator(fModule.fGenerator),
	fDeclaration(declaration),
	fBuilder(fGenerator.fContext),
	fFunction(createFunction(fGenerator, declaration, result))
{
}


/*	createBlock
	Create a basic block
*/
template <typename ...Args>
Generator::Module::Procedure::Block &Generator::Module::Procedure::createBlock(
	Args		&&...args
	)
{
// create the LLVM block
llvm::BasicBlock *block = llvm::BasicBlock::Create(std::forward<Args>(args)...);

// now create the block object in our map
return fDefinitions.try_emplace(block, *this, block).first->second;
}


/*	readVariable
	Emit code to read a value from a given variable
*/
llvm::Value *Generator::Module::Procedure::readVariable(
	const VariableDeclaration &declaration
	)
{
llvm::Value *result;

// variable's enclosing declaration is this procedure?
if (declaration.fEnclosing == &fDeclaration)
	result = Current().read(declaration);

// variable's enclosing declaration is this procedure's module?
else if (declaration.fEnclosing == &fModule.fDeclaration)
	result = fBuilder.CreateLoad(
		fGenerator.mapType(declaration),
		fModule.fGlobals.find(&declaration)->second
		);

else
	throw "unsupported: nested procedures";

return result;
}


/*	readVariable
	Emit code to read a value from a given parameter
*/
llvm::Value *Generator::Module::Procedure::readVariable(
	const FormalParameterDeclaration &declaration
	)
{
llvm::Value *result;

if (declaration.fIsVariable)
	result = fBuilder.CreateLoad(
		fGenerator.mapType(declaration)->getPointerElementType(),
		fFormalParameters.find(&declaration)->second
		);

else
	result = Current().read(declaration);

return result;
}


/*	readVariable
	Emit code to read a value from a given declaration
*/
llvm::Value *Generator::Module::Procedure::readVariable(
	const Declaration &declaration
	)
{
llvm::Value *result;

switch (declaration.fKind) {
	case Declaration::kVariable:
		result = readVariable(llvm::cast<VariableDeclaration>(declaration));
		break;

	case Declaration::kParameter:
		result = readVariable(llvm::cast<FormalParameterDeclaration>(declaration));
		break;
	
	default:
		throw "unsupported declaration";
	}

return result;
}


/*	writeVariable
	Emit code to write a value to a given variable
*/
void Generator::Module::Procedure::writeVariable(
	const VariableDeclaration &declaration,
	llvm::Value	*value
	)
{
// variable's enclosing declaration is this procedure?
if (declaration.fEnclosing == &fDeclaration)
	Current().write(declaration, value);

// variable's enclosing declaration is this procedure's module?
else if (declaration.fEnclosing == &fModule.fDeclaration)
	fBuilder.CreateStore(
		value,
		fModule.fGlobals.find(&declaration)->second
		);

else
	throw "unsupported: nested procedures";
}


/*	writeVariable
	Emit code to write a value to a given formal parameter
*/
void Generator::Module::Procedure::writeVariable(
	const FormalParameterDeclaration &declaration,
	llvm::Value	*value
	)
{
if (declaration.fIsVariable)
	fBuilder.CreateStore(
		value,
		fFormalParameters.find(&declaration)->second
		);

else
	Current().write(declaration, value);
}


/*	writeVariable
	Emit code to write a value to a given declaration
*/
void Generator::Module::Procedure::writeVariable(
	const Declaration &declaration,
	llvm::Value	*value
	)
{
// simulate virtual function dispatch for LLVM-style inheritance
// *** this must be handled carefully to prevent infinite recursion!
switch (declaration.fKind) {
	case Declaration::kVariable:
		writeVariable(llvm::cast<VariableDeclaration>(declaration), value);
		break;
	
	case Declaration::kParameter:
		writeVariable(llvm::cast<FormalParameterDeclaration>(declaration) ,value);
		break;
	
	default:
		throw "unsupported declaration";
	}
}


/*	emit
	Emit code for an infix expression
*/
llvm::Value *Generator::Module::Procedure::emit(
	const InfixExpression &expression
	)
{
llvm::Value
	*const left = emit(*expression.fLeft),
	*const right = emit(*expression.fRight),
	*result;

switch (expression.fOperator.fKind) {
	case Token::kPlus:		result = fBuilder.CreateNSWAdd(left, right); break;
	case Token::kMinus:		result = fBuilder.CreateNSWSub(left, right); break;
	case Token::kAsterisk:		result = fBuilder.CreateNSWMul(left, right); break;
	case Token::keywordDIV:		result = fBuilder.CreateSDiv(left, right); break;
	case Token::keywordMOD:		result = fBuilder.CreateSRem(left, right); break;
	case Token::kEqual:		result = fBuilder.CreateICmpEQ(left, right); break;
	case Token::kHash:		result = fBuilder.CreateICmpNE(left, right); break;
	case Token::kLessThan:		result = fBuilder.CreateICmpSLT(left, right); break;
	case Token::kLessThanEqual:	result = fBuilder.CreateICmpSLE(left, right); break;
	case Token::kGreaterThan:	result = fBuilder.CreateICmpSGT(left, right); break;
	case Token::kGreaterThanEqual:	result = fBuilder.CreateICmpSGE(left, right); break;
	case Token::keywordAND:		result = fBuilder.CreateAnd(left, right); break;
	case Token::keywordOR:		result = fBuilder.CreateOr(left, right); break;
	case Token::kSlash:		throw "wrong operator";
	}

return result;
}


/*	emit
	Emit code for a prefix expression
*/
llvm::Value *Generator::Module::Procedure::emit(
	const PrefixExpression &expression
	)
{
llvm::Value *result = emit(*expression.fExpression);

switch (expression.fOperator.fKind) {
	case Token::kPlus:		break; // identity (nothing to do)
	case Token::kMinus:		result = fBuilder.CreateNeg(result); break;
	case Token::keywordNOT:		result = fBuilder.CreateNot(result); break;
	}

return result;
}


/*	emit
	Emit code for an expression
*/
llvm::Value *Generator::Module::Procedure::emit(
	const Expression &expression
	)
{
llvm::Value *result;

switch (expression.fKind) {
	case Expression::kInfix:
		result = emit(static_cast<const InfixExpression&>(expression));
		break;
	
	case Expression::kPrefix:
		result = emit(static_cast<const PrefixExpression&>(expression));
		break;
	
	case Expression::kVariable:
		result = readVariable(
			*static_cast<const VariableAccess&>(expression).fVariable
			);
		break;
	
	case Expression::kParameter:
		result = readVariable(
			*static_cast<const FormalParameterAccess&>(expression).fParameter
			);
		break;
	
	case Expression::kConstant:
		result = emit(
			*static_cast<const ConstantAccess&>(expression).fConstant->fExpression
			);
		break;
	
	case Expression::kInteger:
		result = llvm::ConstantInt::get(
			fGenerator.fTypeInteger64,
			static_cast<const IntegerLiteral&>(expression).fValue
			);
		break;
	
	case Expression::kBoolean:
		result = llvm::ConstantInt::get(
			fGenerator.fTypeInteger1,
			static_cast<const BooleanLiteral&>(expression).fValue
			);
		break;
	}

return result;
}


/*	emit
	Emit code for an assignment statement
*/
void Generator::Module::Procedure::emit(
	const AssignmentStatement &statement
	)
{
llvm::Value *value = emit(*statement.fExpression);
writeVariable(*statement.fDeclaration, value);
}


/*	emit
	Emit code for a procedure call
*/
void Generator::Module::Procedure::emit(
	const ProcedureCallStatement &statement
	)
{
llvm::report_fatal_error("not implemented");
}


/*	emit
	Emit code for an IF/THEN/ELSE statement
*/
void Generator::Module::Procedure::emit(
	const IfStatement &statement
	)
{
// create the required basic blocks
Block
	&blockIf = createBlock(fGenerator.fContext, "if.body", fFunction),
	*blockElse =
		!statement.fElseStatements.empty() ?
			&createBlock(fGenerator.fContext, "else.body", fFunction) :
			nullptr,
	&blockAfter = createBlock(fGenerator.fContext, "after.if", fFunction);

llvm::Value *condition = emit(*statement.fCondition);
fBuilder.CreateCondBr(condition, blockIf, blockElse ? *blockElse : blockAfter);

Current().seal();
fBuilder.SetInsertPoint(blockIf);
emit(statement.fThenStatements);
if (!fBuilder.GetInsertBlock()->getTerminator())
	fBuilder.CreateBr(blockAfter);

if (blockElse) {
	Current().seal();
	fBuilder.SetInsertPoint(*blockElse);
	
	emit(statement.fElseStatements);
	if (!fBuilder.GetInsertBlock()->getTerminator())
		fBuilder.CreateBr(blockAfter);
	}

Current().seal();
fBuilder.SetInsertPoint(blockAfter);
}


/*	emit
	Emit code for a WHILE statement
*/
void Generator::Module::Procedure::emit(
	const WhileStatement &statement
	)
{
Block
	&blockCondition = createBlock(fGenerator.fContext, "while.cond", fFunction),
	&blockBody = createBlock(fGenerator.fContext, "while.body", fFunction),
	&blockAfter = createBlock(fGenerator.fContext, "after.while", fFunction);

Current().seal();

fBuilder.CreateBr(blockCondition);
fBuilder.SetInsertPoint(blockCondition);

llvm::Value *condition = emit(*statement.fCondition);
fBuilder.CreateCondBr(condition, blockBody, blockAfter);

fBuilder.SetInsertPoint(blockBody);
emit(statement.fStatements);
fBuilder.CreateBr(blockCondition);

blockBody.seal();
blockCondition.seal();

fBuilder.SetInsertPoint(blockAfter);
}


/*	emit
	Emit code for a RETURN statement
*/
void Generator::Module::Procedure::emit(
	const ReturnStatement &statement
	)
{
if (statement.fReturnValue)
	fBuilder.CreateRet(
		emit(*statement.fReturnValue)
		);

else
	fBuilder.CreateRetVoid();
}


/*	emit
	Emit code for a statement
*/
void Generator::Module::Procedure::emit(
	const Statement	&statement
	)
{
// simulate virtual function dispatch for LLVM-style inheritance
// *** this must be handled carefully to prevent infinite recursion!
switch (statement.fKind) {
	case Statement::kAssignment:
		emit(static_cast<const AssignmentStatement&>(statement));
		break;
	
	case Statement::kProcedureCall:
		emit(static_cast<const ProcedureCallStatement&>(statement));
		break;
	
	case Statement::kIf:
		emit(static_cast<const IfStatement&>(statement));
		break;
	
	case Statement::kWhile:
		emit(static_cast<const WhileStatement&>(statement));
		break;
	
	case Statement::kReturn:
		emit(static_cast<const ReturnStatement&>(statement));
		break;
	}
}


/*	emit
	Emit code for a statement list
*/
void Generator::Module::Procedure::emit(
	const Statements &statements
	)
{
// emit each statement in the list
for (const Statement *const statement: statements)
	emit(*statement);
}


/*	()
	Emit code for a procedure declaration
*/
void Generator::Module::Procedure::operator()()
{
Block &block = createBlock(fGenerator.fContext, "entry", fFunction);
fBuilder.SetInsertPoint(block);

// parallel iteration over parameters/arguments
for (unsigned argumentI = 0; argumentI < fFunction->arg_size(); argumentI++) {
	llvm::Argument &argument = *fFunction->getArg(argumentI);
	FormalParameterDeclaration &parameterDeclaration = *fDeclaration.fParameters[argumentI];
	
	// create mapping of FormalParameter to llvm::Argument for VAR parameters
	fFormalParameters[&parameterDeclaration] = &argument;
	
	block.fDefinitions.try_emplace(&parameterDeclaration, &argument);
	}

// emit variable declarations
for (const Declaration *const declaration: fDeclaration.fDeclarations)
	if (const VariableDeclaration *const variableDeclaration = llvm::dyn_cast<VariableDeclaration>(declaration))
		if (
			llvm::Type *type = fGenerator.mapType(*variableDeclaration);
			type->isAggregateType()
			)
			block.fDefinitions.try_emplace(
				variableDeclaration,
				fBuilder.CreateAlloca(type)
				);

// emit procedure statements
emit(fDeclaration.fStatements);

// make sure block has a terminating instruction?
if (fBuilder.GetInsertBlock()->getTerminator())
	fBuilder.CreateRetVoid();

Current().seal();
}



/*

	module generator

*/

/*	emit
	Emit code for a module-level ('global') variable declaration
*/
void Generator::Module::emit(
	const VariableDeclaration &declaration,
	llvm::Module	&module
	)
{
// emit code to create global variable
(void) fGlobals.try_emplace(
	&declaration,
	new llvm::GlobalVariable(
		module,
		fGenerator.mapType(*declaration.fType),
		false /* isConstant */,
		llvm::GlobalValue::PrivateLinkage,
		nullptr,
		fGenerator.mangleName(declaration)
		)
	);
}


/*	emit
	Emit code for a procedure declaration
*/
void Generator::Module::emit(
	const ProcedureDeclaration &declaration,
	llvm::Module	&module
	)
{
// emit the procedure
Procedure::Generate(Procedure(*this, declaration, module));
}


/*	emit
	Emit code for the given module-level declaration
*/
void Generator::Module::emit(
	const Declaration &declaration,
	llvm::Module	&module
	)
{
// simulate virtual function dispatch for LLVM-style inheritance
// *** this must be handled carefully to prevent infinite recursion!
switch (declaration.fKind) {
	case Declaration::kVariable:
		emit(
			llvm::cast<const VariableDeclaration>(declaration),
			module
			);
		break;
	
	case Declaration::kProcedure:
		emit(
			llvm::cast<const ProcedureDeclaration>(declaration),
			module
			);
		break;
	}
}


/*	()
	Generate module code into the given file
*/
std::unique_ptr<llvm::Module> Generator::Module::operator()(
	const std::filesystem::path &file
	)
{
// create "LLVM module"
std::unique_ptr<llvm::Module> module = std::make_unique<llvm::Module>(file.native(), fGenerator.fContext);
module->setTargetTriple(fTargetMachine.getTargetTriple().getTriple());
module->setDataLayout(fTargetMachine.createDataLayout());

// for each declaration
for (const Declaration *const declaration: fDeclaration.fDeclarations)
	// emit code for the declaration
	emit(*declaration, *module);

return module;
}



/*

	generator

*/

/*	Generator
	Prepare to generate code
*/
Generator::Generator(
	llvm::LLVMContext &context
	) :
	fContext(context),
	fTypeVoid(llvm::Type::getVoidTy(context)),
	fTypeInteger1(llvm::Type::getInt1Ty(context)),
	fTypeInteger32(llvm::Type::getInt32Ty(context)),
	fTypeInteger64(llvm::Type::getInt64Ty(context)),
	fInteger32Zero(llvm::ConstantInt::get(fTypeInteger32, 0, true /* isSigned */))
{
}


/*	mapType
	Return the LLVM IR type for the type declaration
*/
llvm::Type *Generator::mapType(
	const TypeDeclaration &declaration
	)
{
llvm::Type *const result =
	declaration.fName == "BOOLEAN" ? fTypeInteger1 :
	declaration.fName == "INTEGER" ? fTypeInteger64 :
	nullptr;
if (!result) llvm::report_fatal_error("unsupported type");

return result;
}


/*	mapType
	Return the LLVM IR type for the declaration
*/
llvm::Type *Generator::mapType(
	const Declaration &declaration
	)
{
llvm::Type *type;

switch (declaration.fKind) {
	case Declaration::kParameter: {
		const FormalParameterDeclaration &formalParameter = llvm::cast<FormalParameterDeclaration>(declaration);
		
		type = mapType(*formalParameter.fType);
		if (formalParameter.fIsVariable)
			type = type->getPointerTo();
		}
		break;
	
	case Declaration::kVariable:
		type = mapType(*llvm::cast<VariableDeclaration>(declaration).fType);
		break;
	
	case Declaration::kType:
		type = mapType(llvm::cast<TypeDeclaration>(declaration));
		break;
	}

return type;
}


/*	mangleName
	Return a linker-safe 'mangled name' for the declaration
*/
std::string Generator::mangleName(
	const Declaration &declaration
	)
{
std::string result;

// ascend up the chain of enclosing declarations
for (const Declaration *d = &declaration; d; d = d->fEnclosing) {
	llvm::SmallString<16> component;
	
	// prepend enclosing declaration's ASCII-encoded length and name
	llvm::StringRef name = d->fName;
	component.append(llvm::itostr(name.size()));
	component.append(name);
	
	result.insert(0, component.c_str());
	}

// prefix
result.insert(0, "_t");

return result;
}
