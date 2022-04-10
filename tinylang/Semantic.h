#pragma once

#include <llvm/ADT/StringMap.h>

#include "AST.h"
#include "Diagnostic.h"


class Semantics {
public:
	/*	Scope
	
	*/
	class Scope {
		friend class Semantics;
	
	protected:
		Semantics	&fSemantics;
		Scope		*const fParent;
		
		Declaration	*const fDeclaration;
		llvm::StringMap<Declaration*> fSymbols;
		
		
				Scope(Semantics&);
		
		bool		insert(Declaration*);
		Declaration	*lookup(llvm::StringRef);
	
	public:
				Scope(Semantics&, Declaration&);
				~Scope();
		};

protected:
	static TypeDeclaration
			*gTypeBoolean,
			*gTypeInteger;
	static BooleanLiteral
			*gLiteralFalse,
			*gLiteralTrue;
	static ConstantDeclaration
			*gConstantFalse,
			*gConstantTrue;
	
	Diagnostics	&fDiagnostics;
	Scope		fGlobalScope,
			*fScope;
	
	
	void		checkFormalAndActualParameters(llvm::SMLoc, const FormalParameterDeclarations&, const Expressions&);
	bool		isOperatorForType(Token::Kind, TypeDeclaration*);

public:
			Semantics(Diagnostics&);
	
	ModuleDeclaration *moduleDeclaration(const Token&);
	void		moduleDeclaration(ModuleDeclaration*, const Token&, Declarations&&, Statements&&);
	Declaration	*constantDeclaration(const Token&, Expression*);
	Declaration	*qualifiedIdentifierPart(Declaration*, const Token&);
	Declarations	variableDeclaration(std::vector<Token>&, Declaration*);
	ProcedureDeclaration *procedureDeclaration(const Token&);
	void		procedureDeclaration(ProcedureDeclaration*, const Token&, Declarations&&, Statements&&);
	void		procedureHeading(ProcedureDeclaration*, FormalParameterDeclarations&&, Declaration*);
	FormalParameterDeclarations formalParameterDeclaration(std::vector<Token>&, Declaration*, bool isVariable);
	Statement	*assignment(const Token&, Declaration*, Expression*);
	Statement	*procedureCall(const Token&, Declaration*, Expressions&&);
	Statement	*ifStatement(const Token&, Expression*, Statements&&, Statements&&);
	Statement	*whileStatement(const Token&, Expression*, Statements&&);
	Statement	*returnStatement(const Token&, Expression*);
	Expression	*integerLiteral(const Token&);
	Expression	*functionCall(Declaration*, Expressions&&);
	Expression	*variable(Declaration*);
	Expression	*prefixExpression(Expression*, OperatorInfo);
	Expression	*term(Expression*, Expression*, OperatorInfo);
	Expression	*simpleExpression(Expression*, Expression*, OperatorInfo);
	Expression	*expression(Expression*, Expression*, OperatorInfo);
	};
