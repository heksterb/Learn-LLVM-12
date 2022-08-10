#include <llvm/ADT/StringRef.h>

#include "Parser.h"




struct SyntaxError {};


Parser::Parser(
	Diagnostics	&diagnostics,
	llvm::StringRef	buffer
	) :
	fDiagnostics(diagnostics),
	fLexer(diagnostics, buffer),
	fActions(diagnostics)
{
advance();
}


/*	advance

*/
void Parser::advance()
{
fToken = fLexer.next();
}


/*	advanceIf
	If the current token is 'kind', then advance and return 'true'
*/
bool Parser::advanceIf(
	Token::Kind	kind
	)
{
bool willAdvance = fToken.kind() == kind;
if (willAdvance) advance();

return willAdvance;
}


/*	consume

*/
void Parser::consume(
	Token::Kind	kind
	)
{
if (fToken.kind() != kind) throw SyntaxError();

advance();
}


/*	expect

*/
Token Parser::expect(
	Token::Kind	kind
	)
{
if (fToken.kind() != kind) {
    // There must be a better way!
	const char *expected = Token::getPunctuatorSpelling(kind);
	if (!expected) expected = Token::getKeywordSpelling(kind);
	
	llvm::StringRef actual(fToken.location().getPointer(), fToken.length());
	fDiagnostics.report(fToken.location(), Diagnostics::errorExpected, llvm::StringRef(expected), actual);
	
	throw SyntaxError();
	}

Token result = fToken;
advance();
return result;
}


/*	recover
	Consume until a token in the follow set
*/
bool Parser::recover(
	std::initializer_list<Token::Kind> follow
	)
{
bool atEnd = false;

while (!fToken.isOneOf(follow)) {
	advance();
	
	if (fToken.kind() == Token::kEOF) {
		atEnd = true;
		break;
		}
	}

return atEnd;
}


OperatorInfo Parser::parseRelation()
{
OperatorInfo relation;

try {
	if (!fToken.isOneOf({
		Token::kEqual, Token::kHash,
		Token::kLessThan, Token::kLessThanEqual,
		Token::kGreaterThan, Token::kGreaterThanEqual
		}))
		throw SyntaxError();
	
	relation = OperatorInfo(fToken.location(), fToken.kind());
	
	advance();
	}

catch (SyntaxError&) {
	if (recover({
		Token::kParenthesisLeft, Token::kPlus, Token::kMinus,
		Token::keywordNOT, Token::kIdentifier,
		Token::kIntegerLiteral
		})) throw;
	}

return relation;
}


OperatorInfo Parser::parseMultiplicationOperator()
{
OperatorInfo op;

try {
	if (!fToken.isOneOf({
		Token::kAsterisk, Token::kSlash,
		Token::keywordDIV, Token::keywordMOD, Token::keywordAND
		}))
		throw SyntaxError();
	
	op = OperatorInfo(fToken.location(), fToken.kind());
	
	advance();
	}

catch (SyntaxError&) {
	if (recover({
		Token::kParenthesisLeft, Token::keywordNOT,
		Token::kIdentifier,
		Token::kIntegerLiteral
		})) throw;
	}

return op;
}


OperatorInfo Parser::parseAddOperator()
{
OperatorInfo op;

try {
	if (!fToken.isOneOf({
		Token::kPlus, Token::kMinus, Token::keywordOR
		}))
		throw SyntaxError();
	
	op = OperatorInfo(fToken.location(), fToken.kind());
	
	advance();
	}

catch (SyntaxError&) {
	if (recover({
		Token::kParenthesisLeft, Token::keywordNOT,
		Token::kIdentifier,
		Token::kIntegerLiteral
		})) throw;
	}

return op;
}


LeftValue *Parser::parseSelectors(
	LeftValue	*expression
	)
{
try {
	switch (Token token = fToken; token.kind()) {
		case Token::kCaret:
			expression = fActions.dereferenceSelector(token, expression);
			
			advance();
			break;
		
		case Token::kBracketLeft: {
			advance();
			
			Expression *indexExpression = parseExpression();
			
			expect(Token::kBracketRight);
			
			expression = fActions.indexSelector(token, expression, indexExpression);
			}
			break;
		
		case Token::kPeriod: {
			advance();
			
			Token identifier = expect(Token::kIdentifier);
			
			expression = fActions.fieldSelector(token, expression, identifier.identifier());
			}
			break;
		}
	}

catch (SyntaxError&) {
	if (recover({
		Token::kHash, Token::kParenthesisRight, Token::kAsterisk, Token::kPlus,
		Token::kComma, Token::kMinus, Token::kSlash, Token::kColonEqual,
		Token::kSemicolon, Token::kLessThan, Token::kLessThanEqual, Token::kEqual,
		Token::kGreaterThan, Token::kBracketRight,
		Token::keywordAND, Token::keywordDIV, Token::keywordDO, Token::keywordELSE,
		Token::keywordMOD, Token::keywordOR, Token::keywordTHEN
		})) throw;
	}

return expression;
}


Expression *Parser::parseFactorFromIdentifier()
{
Expression *factor;

Declaration *declaration = parseQualifiedIdentifier();
	if (!declaration) return nullptr;

if (advanceIf(Token::kParenthesisLeft)) {
	Expressions expressions;
	
	if (fToken.isOneOf({
		Token::kParenthesisLeft, Token::kPlus, Token::kMinus,
		Token::keywordNOT, Token::kIdentifier,
		Token::kIntegerLiteral
		}))
		expressions = parseExpressionList();
	
	consume(Token::kParenthesisRight);
	
	factor = fActions.functionCall(declaration, std::move(expressions));
	}

else if (declaration->fKind == Declaration::kConstant)
	factor = fActions.constant(declaration);

else {
	LeftValue *designator = fActions.designator(declaration);
	
	factor = parseSelectors(designator);
	}

return factor;
}


Expression *Parser::parseFactor()
{
Expression *factor;

try {
	switch (fToken.kind()) {
		case Token::kIntegerLiteral: {
			Token token = fToken;
			advance();
			
			factor = fActions.integerLiteral(token);
			}
			break;
	
		case Token::kIdentifier:
			factor = parseFactorFromIdentifier();
			break;
		
		case Token::kParenthesisLeft:
			advance();
			
			factor = parseExpression();
			consume(Token::kParenthesisRight);
			
			break;
	
		case Token::keywordNOT: {
			OperatorInfo op(fToken.location(), fToken.kind());
			
			advance();
			Expression *expression = parseFactor();
			factor = fActions.prefixExpression(expression, op);
			}
			break;
		
		default:
			throw SyntaxError();
		}
	}

catch (SyntaxError&) {
	if (recover({
		Token::kHash, Token::kParenthesisRight, Token::kAsterisk, Token::kPlus,
		Token::kComma, Token::kMinus, Token::kSlash, Token::kSemicolon,
		Token::kLessThan, Token::kLessThanEqual, Token::kEqual, Token::kGreaterThan,
		Token::kGreaterThanEqual, Token::kBracketRight,
		Token::keywordAND, Token::keywordDIV,
		Token::keywordDO, Token::keywordELSE, Token::keywordEND, Token::keywordMOD,
		Token::keywordOR, Token::keywordTHEN
		})) throw;
	
	factor = nullptr;
	}

return factor;
}


Expression *Parser::parseTerm()
{
Expression *term;

try {
	term = parseFactor();
	
	while (fToken.isOneOf({
		Token::kAsterisk, Token::kSlash,
		Token::keywordAND, Token::keywordDIV, Token::keywordMOD
		})) {
		OperatorInfo op = parseMultiplicationOperator();
		
		Expression *right = parseFactor();
		
		term = fActions.term(term, right, op);
		}
	}

catch (SyntaxError&) {
	if (recover({
		Token::kParenthesisRight, Token::kBracketRight,
		Token::kHash, Token::kComma,
		Token::kPlus, Token::kMinus, Token::kSemicolon,
		Token::kLessThan, Token::kLessThanEqual, Token::kEqual,
		Token::kGreaterThan, Token::kGreaterThanEqual,
		Token::keywordDO, Token::keywordELSE, Token::keywordEND,
		Token::keywordOR, Token::keywordTHEN
		})) throw;
	
	term = nullptr;
	}

return term;
}


Expression *Parser::parseSimpleExpression()
{
Expression *expression;

try {
	OperatorInfo prefix;
	
	if (fToken.isOneOf({ Token::kPlus, Token::kMinus }))
		prefix = OperatorInfo(fToken.location(), fToken.kind());
	
	expression = parseTerm();
	
	while (fToken.isOneOf({ Token::kPlus, Token::kMinus, Token::keywordOR })) {
		OperatorInfo op = parseAddOperator();
		Expression *right = parseTerm();
		
		expression = fActions.simpleExpression(expression, right, op);
		}
	
	if (!prefix.fIsUnspecified)
		expression = fActions.prefixExpression(expression, prefix);
	}

catch (SyntaxError&) {
	if (recover({
		Token::kParenthesisRight, Token::kBracketRight,
		Token::kHash, Token::kComma, Token::kSemicolon,
		Token::kLessThan, Token::kLessThanEqual, Token::kEqual, Token::kGreaterThan,
		Token::kGreaterThanEqual, Token::keywordDO, Token::keywordELSE,
		Token::keywordEND, Token::keywordTHEN
		})) throw;
	
	expression = nullptr;
	}

return expression;
}


Expression *Parser::parseExpression()
{
Expression *expression;

try {
	expression = parseSimpleExpression();
	
	if (fToken.isOneOf({
		Token::kHash, Token::kLessThan, Token::kLessThanEqual,
		Token::kEqual, Token::kGreaterThan, Token::kGreaterThanEqual
		})) {
		OperatorInfo op = parseRelation();
		
		Expression *right = parseSimpleExpression();
		
		expression = fActions.expression(expression, right, op);
		}
	}

catch (SyntaxError&) {
	if (recover({
		Token::kParenthesisRight, Token::kBracketRight,
		Token::kComma, Token::kSemicolon,
		Token::keywordDO, Token::keywordELSE, Token::keywordEND, Token::keywordTHEN
		})) throw;
	
	expression = nullptr;
	}

return expression;
}


Expressions Parser::parseExpressionList()
{
Expressions expressions;

try {
	do
		if (Expression *expression = parseExpression())
			expressions.push_back(expression);
		
		while (advanceIf(Token::kComma));
	}

catch (SyntaxError&) {
	if (recover({ Token::kParenthesisRight })) throw;
	}

return expressions;
}


std::vector<Token> Parser::parseIdentifierList()
{
std::vector<Token> identifiers;

try {
	do {
		identifiers.push_back(
			expect(Token::kIdentifier)
			);
		} while (advanceIf(Token::kComma));
	}

catch (SyntaxError&) {
	if (recover({ Token::kColon, Token::kSemicolon })) throw;
	}

return identifiers;
}


void Parser::parseImport()
{
try {
	llvm::StringRef moduleName;
	
	if (advanceIf(Token::keywordFROM)) {
		Token token = expect(Token::kIdentifier);
		moduleName = token.identifier();
		}
	
	consume(Token::keywordIMPORT);
	
	std::vector<Token> ids = parseIdentifierList();
	consume(Token::kSemicolon);
	// fActions.import(moduleName, ids);
	}

catch (const SyntaxError&) {
	if (recover({
		Token::keywordBEGIN, Token::keywordCONST,
		Token::keywordEND, Token::keywordFROM,
		Token::keywordIMPORT, Token::keywordPROCEDURE,
		Token::keywordTYPE, Token::keywordVAR
		})) throw;
	}
}


ParameterDeclarations Parser::parseParameter()
{
ParameterDeclarations parameters;

try {
	// parse VAR
	const bool isVariable = advanceIf(Token::keywordVAR);
	
	// parse identifiers
	std::vector<Token> identifiers = parseIdentifierList();
	consume(Token::kColon);
	
	// parse type declaration
	Declaration *typeDeclaration = parseQualifiedIdentifier();
	
	parameters = fActions.parameterDeclaration(identifiers, typeDeclaration, isVariable);
	}

catch (SyntaxError&) {
	if (recover({ Token::kParenthesisRight, Token::kSemicolon })) throw;
	}

return parameters;
}


ParameterDeclarations Parser::parseParameterList()
{
ParameterDeclarations parameters;

try {
	do {
		// parse the formal parameter
		ParameterDeclarations parameter = parseParameter();
		
		// append to all formal parameters
		parameters.insert(parameters.end(), parameter.begin(), parameter.end());
		} while (advanceIf(Token::kSemicolon));
	}

catch (SyntaxError&) {
	if (recover({ Token::kParenthesisRight })) throw;
	}

return parameters;
}


std::pair<ParameterDeclarations, Declaration*> Parser::parseParameters()
{
std::pair<ParameterDeclarations, Declaration*> parameters;

try {
	consume(Token::kParenthesisLeft);
	
	if (fToken.isOneOf({ Token::keywordVAR, Token::kIdentifier }))
		parameters.first = parseParameterList();
	
	consume(Token::kParenthesisRight);
	
	if (advanceIf(Token::kColon))
		parameters.second = parseQualifiedIdentifier();
	}

catch (SyntaxError&) {
	if (recover({ Token::kSemicolon })) throw;
	}

return parameters;
}


Declaration *Parser::parseConstantDeclaration()
{
Declaration *declaration;

try {
	Token token = expect(Token::kIdentifier);
	
	consume(Token::kEqual);
	
	Expression *const expression = parseExpression();
	
	declaration = fActions.constantDeclaration(token, expression);
	}

catch (SyntaxError&) {
	if (recover({ Token::kSemicolon })) throw;
	
	declaration = nullptr;
	}

return declaration;
}


RecordTypeDeclaration::Fields Parser::parseField()
{
RecordTypeDeclaration::Fields result;

try {
	std::vector<Token> identifiers = parseIdentifierList();
	
	consume(Token::kColon);
	
	Declaration *d = parseQualifiedIdentifier();
	
	result = fActions.fieldDeclaration(identifiers, d);
	}

catch (SyntaxError&) {
	if (recover({ Token::kSemicolon })) throw;
	}

return result;
}


RecordTypeDeclaration::Fields Parser::parseFieldList()
{
RecordTypeDeclaration::Fields fields;

try {
	do {
		RecordTypeDeclaration::Fields field = parseField();
		
		fields.insert(fields.end(), field.begin(), field.end());
		} while (advanceIf(Token::kSemicolon));
	}

catch (SyntaxError&) {
	if (recover({ Token::keywordEND })) throw;
	}

return fields;
}


// *** why not return TypeDeclaration
Declaration *Parser::parseTypeDeclaration()
{
Declaration *declaration;

try {
	Token token = expect(Token::kIdentifier);
	
	consume(Token::kEqual);
	
	switch (fToken.kind()) {
		case Token::kIdentifier: {
			Declaration *d = parseQualifiedIdentifier();
			
			declaration = fActions.aliasTypeDeclaration(token, d);
			}
			break;
		
		case Token::keywordPOINTER: {
			advance();
			consume(Token::keywordTO);
			
			Declaration *d = parseQualifiedIdentifier();
			
			declaration = fActions.pointerTypeDeclaration(token, d);
			}
			break;
		
		case Token::keywordARRAY: {
			advance();
			consume(Token::kBracketLeft);
			
			Expression *expression = parseExpression();
			
			consume(Token::kBracketRight);
			consume(Token::keywordOF);
			
			Declaration *d = parseQualifiedIdentifier();
			
			declaration = fActions.arrayTypeDeclaration(token, expression, d);
			}
			break;
		
		case Token::keywordRECORD: {
			advance();
			RecordTypeDeclaration::Fields fields = parseFieldList();
			
			consume(Token::keywordEND);
			
			declaration = fActions.recordTypeDeclaration(token, std::move(fields));
			}
			break;
			
		default:
			throw SyntaxError();
		}
	}

catch (SyntaxError&) {
	if (recover({ Token::kSemicolon })) throw;
	
	declaration = nullptr;
	}

return declaration;
}


Declaration *Parser::parseQualifiedIdentifier()
{
Declaration *declaration = nullptr;

try {
	do {
		Token token = expect(Token::kIdentifier);
		
		declaration = fActions.qualifiedIdentifierPart(declaration, token);
		if (!declaration) return nullptr;
		}
		while (declaration->fKind == Declaration::kModule && advanceIf(Token::kPeriod));
	}

catch (SyntaxError&) {
	if (recover({
		Token::kHash, Token::kParenthesisLeft, Token::kParenthesisRight, Token::kAsterisk,
		Token::kPlus, Token::kComma, Token::kMinus, Token::kSlash,
		Token::kColonEqual, Token::kSemicolon, Token::kLessThan, Token::kLessThanEqual,
		Token::kEqual, Token::kGreaterThan, Token::kGreaterThanEqual,
		Token::kBracketLeft, Token::kBracketRight, Token::kCaret,
		Token::keywordAND, Token::keywordDIV, Token::keywordDO, Token::keywordELSE,
		Token::keywordEND, Token::keywordMOD, Token::keywordOR, Token::keywordTHEN
		})) throw;
	
	declaration = nullptr;
	}

return declaration;
}


Declarations Parser::parseVariableDeclaration()
{
Declarations declarations;

try {
	std::vector<Token> identifiers = parseIdentifierList();
	
	consume(Token::kColon);
	
	Declaration *identifier = parseQualifiedIdentifier();
	if (!identifier) return {};
	
	declarations = fActions.variableDeclaration(identifiers, identifier);
	}

catch (SyntaxError&) {
	if (recover({ Token::kSemicolon })) throw;
	}

return declarations;
}


Declaration *Parser::parseProcedureDeclaration()
{
ProcedureDeclaration *procedure;

try {
	Token token = expect(Token::kIdentifier);
	procedure = fActions.procedureDeclaration(token);
	
	Semantics::Scope scope(fActions, *procedure);
	
	{
		std::pair<ParameterDeclarations, Declaration*> parameters;
		
		if (fToken.kind() == Token::kParenthesisLeft)
			parameters = parseParameters();
		
		fActions.procedureHeading(procedure, std::move(parameters.first), parameters.second);
		}
	
	consume(Token::kSemicolon);
	
	std::pair<Declarations, Statements> block = parseBlock();
	
	Token token2 = expect(Token::kIdentifier);
	fActions.procedureDeclaration(procedure, token2, std::move(block.first), std::move(block.second));
	}

catch (SyntaxError&) {
	if (recover({ Token::kSemicolon })) throw;
	
	procedure = nullptr;
	}

return procedure;
}


Declarations Parser::parseDeclaration()
{
Declarations declarations;

try {
	if (advanceIf(Token::keywordCONST))
		while (fToken.kind() == Token::kIdentifier) {
			if (Declaration *const declaration = parseConstantDeclaration())
				declarations.push_back(declaration);
			
			consume(Token::kSemicolon);
			}
	
	else if (advanceIf(Token::keywordTYPE))
		while (fToken.kind() == Token::kIdentifier) {
			if (Declaration *const declaration = parseTypeDeclaration())
				declarations.push_back(declaration);
			
			consume(Token::kSemicolon);
			}
	
	else if (advanceIf(Token::keywordVAR))
		while (fToken.kind() == Token::kIdentifier) {
			declarations = parseVariableDeclaration();
			
			consume(Token::kSemicolon);
			}
	
	else if (advanceIf(Token::keywordPROCEDURE)) {
		declarations.push_back(
			parseProcedureDeclaration()
			);
		
		consume(Token::kSemicolon);
		}
	
	else
		/*ERROR*/
		throw SyntaxError();
	}

catch (SyntaxError&) {
	if (recover({
		Token::keywordBEGIN, Token::keywordCONST,
		Token::keywordEND, Token::keywordPROCEDURE,
		Token::keywordTYPE, Token::keywordVAR
		})) throw;
	}

return declarations;
}


Statement *Parser::parseIfStatement()
{
Statement *statement;

try {
	Token token = expect(Token::keywordIF);
	
	Expression *expression = parseExpression();
	
	consume(Token::keywordTHEN);
	
	Statements
		thenStatements = parseStatementSequence(),
		elseStatements;
	
	if (advanceIf(Token::keywordELSE))
		elseStatements = parseStatementSequence();
	
	consume(Token::keywordEND);
	
	statement = fActions.ifStatement(token, expression, std::move(thenStatements), std::move(elseStatements));
	}

catch (SyntaxError&) {
	if (recover({
		Token::kSemicolon, Token::keywordELSE, Token::keywordEND
		})) throw;
	
	statement = nullptr;
	}

return statement;
}


Statement *Parser::parseWhileStatement()
{
Statement *statement;

try {
	Token token = expect(Token::keywordWHILE);
	
	Expression *expression = parseExpression();
	
	consume(Token::keywordDO);
	
	Statements whileStatements = parseStatementSequence();
	
	consume(Token::keywordEND);
	
	statement = fActions.whileStatement(token, expression, std::move(whileStatements));
	}

catch (SyntaxError&) {
	if (recover({
		Token::kSemicolon, Token::keywordELSE, Token::keywordEND
		})) throw;
	
	statement = nullptr;
	}

return statement;
}


Statement *Parser::parseReturnStatement()
{
Statement *statement;

try {
	Token token = expect(Token::keywordRETURN);
	
	if (!fToken.isOneOf({
		Token::kParenthesisLeft,
		Token::kPlus, Token::kMinus,
		Token::keywordNOT, // *** why not Boolean literal?
		Token::kIdentifier, Token::kIntegerLiteral
		}))
		throw SyntaxError();
	
	Expression *expression = parseExpression();
	
	statement = fActions.returnStatement(token, expression);
	}

catch (SyntaxError&) {
	if (recover({
		Token::kSemicolon, Token::keywordELSE, Token::keywordEND
		})) throw;
	
	statement = nullptr;
	}

return statement;
}


Statement *Parser::parseStatementOfVariables()
{
Statement *statement;

Token token = fToken;

Declaration *declaration = parseQualifiedIdentifier();

if (advanceIf(Token::kParenthesisLeft)) {
	Expressions expressions;
	if (fToken.isOneOf({
		Token::kParenthesisLeft, Token::kPlus,
		Token::kMinus, Token::keywordNOT,
		Token::kIdentifier,
		Token::kIntegerLiteral
		}))
		expressions = parseExpressionList();
	
	consume(Token::kParenthesisRight);
	
	statement = fActions.procedureCall(token, declaration, std::move(expressions));
	}

else {
	LeftValue *designator = fActions.designator(declaration);
	
	designator = parseSelectors(designator);
	
	consume(Token::kColonEqual);
	
	Expression *expression = parseExpression();
	
	statement =
		expression ? fActions.assignment(token, designator, expression) :
		nullptr;
	}

return statement;
}


Statement *Parser::parseStatement()
{
Statement *statement;

try {
	switch (fToken.kind()) {
		case Token::kIdentifier:
			statement = parseStatementOfVariables();
			break;
		
		case Token::keywordIF:
			statement = parseIfStatement();
			break;
		
		case Token::keywordWHILE:
			statement = parseWhileStatement();
			break;
		
		case Token::keywordRETURN:
			statement = parseReturnStatement();
			break;
		
		default:
			/*ERROR*/
			throw SyntaxError();
		}
	}

catch (SyntaxError&) {
	if (recover({ Token::kSemicolon, Token::keywordELSE, Token::keywordEND })) throw;
	
	statement = nullptr;
	}

return statement;
}


Statements Parser::parseStatementSequence()
{
Statements statements;

try {
	do
		if (Statement *statement = parseStatement())
			statements.push_back(statement);
		
		while (advanceIf(Token::kSemicolon));
	}

catch (SyntaxError&) {
	if (recover({ Token::keywordELSE, Token::keywordEND })) throw;
	}

return statements;
}


std::pair<Declarations, Statements> Parser::parseBlock()
{
std::pair<Declarations, Statements> block;
Declarations &resultDeclarations = block.first;
Statements &resultStatements = block.second;

try {
	while (fToken.isOneOf({
		Token::keywordCONST, Token::keywordPROCEDURE,
		Token::keywordTYPE, Token::keywordVAR
		}))
		resultDeclarations.append(parseDeclaration());
	
	if (advanceIf(Token::keywordBEGIN))
		resultStatements = parseStatementSequence();
	
	consume(Token::keywordEND);
	}

catch (SyntaxError&) {
	if (recover({ Token::kIdentifier })) throw;
	}

return block;
}


ModuleDeclaration *Parser::parseCompilationUnit()
{
ModuleDeclaration *module;

try {
	// MODULE declaration
	consume(Token::keywordMODULE);
	
	Token token = expect(Token::kIdentifier);
	module = fActions.moduleDeclaration(token);
	
	// open module scope
	Semantics::Scope scope(fActions, *module);
	consume(Token::kSemicolon);
	
	// IMPORT declarations
	while (fToken.isOneOf({ Token::keywordFROM, Token::keywordIMPORT }))
		parseImport();
	
	// body block (consumes END)
	std::pair<Declarations, Statements> block = parseBlock();
	
	// end of module
	Token token2 = expect(Token::kIdentifier);
	consume(Token::kPeriod);
	
	fActions.moduleDeclaration(module, token2, std::move(block.first), std::move(block.second));
	}

catch (SyntaxError&) {
	(void) recover({});
	
	module = nullptr;
	}

return module;
}


ModuleDeclaration *Parser::operator()()
{
return parseCompilationUnit();
}
