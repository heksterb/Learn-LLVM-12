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
	static PervasiveTypeDeclaration
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
	
	
	void		checkFormalAndActualParameters(llvm::SMLoc, const ParameterDeclarations&, const Expressions&);
	bool		isOperatorForType(Token::Kind, TypeDeclaration*);

public:
			Semantics(Diagnostics&);
	
	ModuleDeclaration *moduleDeclaration(const Token&);
	void		moduleDeclaration(ModuleDeclaration*, const Token&, Declarations&&, Statements&&);
	Declaration	*constantDeclaration(const Token&, Expression*);
	Declaration	*qualifiedIdentifierPart(Declaration*, const Token&);
	Declarations	variableDeclaration(const std::vector<Token>&, Declaration*);
	ProcedureDeclaration *procedureDeclaration(const Token&);
	void		procedureDeclaration(ProcedureDeclaration*, const Token&, Declarations&&, Statements&&);
	void		procedureHeading(ProcedureDeclaration*, ParameterDeclarations&&, Declaration*);
	ParameterDeclarations parameterDeclaration(const std::vector<Token>&, Declaration*, bool isVariable);
	Statement	*assignment(const Token&, LeftValue*, Expression*);
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
	Designator	*dereferenceSelector(const Token&, LeftValue*);
	Designator	*indexSelector(const Token&, LeftValue*, Expression*);
	Designator	*fieldSelector(const Token&, LeftValue*, llvm::StringRef);
	LeftValue	*designator(Declaration*);
	Expression	*constant(Declaration*);
	TypeDeclaration	*aliasTypeDeclaration(const Token&, Declaration*);
	TypeDeclaration	*pointerTypeDeclaration(const Token&, Declaration*);
	TypeDeclaration	*arrayTypeDeclaration(const Token&, Expression*, Declaration*);
	TypeDeclaration	*recordTypeDeclaration(const Token&, RecordTypeDeclaration::Fields&&);
	RecordTypeDeclaration::Fields fieldDeclaration(const std::vector<Token>&, Declaration*);
	};
