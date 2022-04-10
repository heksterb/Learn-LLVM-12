#pragma once

#include <initializer_list>

#include "AST.h"
#include "Lexer.h"
#include "Semantic.h"
#include "Tokens.h"


class Parser {
protected:
	Diagnostics	&fDiagnostics;
	Lexer		fLexer;
	Semantics	fActions;
	Token		fToken;
	
	
	void		advance();
	bool		advanceIf(Token::Kind);
	void		consume(Token::Kind);
	Token		expect(Token::Kind);
	bool		recover(std::initializer_list<Token::Kind> follow);

	OperatorInfo	parseRelation(),
			parseAddOperator(),
			parseMultiplicationOperator();
	Expression	*parseFactorFromIdentifier(),
			*parseFactor(),
			*parseTerm(),
			*parseSimpleExpression(),
			*parseExpression();
	Expressions	parseExpressionList();
	std::vector<Token> parseIdentifierList();
	void		parseImport();
	FormalParameterDeclarations
			parseFormalParameterList(),
			parseFormalParameter();
	std::pair<FormalParameterDeclarations, Declaration*>
			parseFormalParameters();
	Declarations	parseVariableDeclaration();
	Declaration	*parseConstantDeclaration(),
			*parseQualifiedIdentifier(),
			*parseProcedureDeclaration();
	Declarations	parseDeclaration();
	Statement	*parseIfStatement(),
			*parseWhileStatement(),
			*parseReturnStatement(),
			*parseStatementOfVariables(),
			*parseStatement();
	Statements	parseStatementSequence();
	std::pair<Declarations, Statements> parseBlock();
	ModuleDeclaration *parseCompilationUnit();

public:
			Parser(Diagnostics&, llvm::StringRef);
	
	ModuleDeclaration *operator()();
	};
