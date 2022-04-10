#pragma once

#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>

#include "Tokens.h"


struct Declaration;
struct Expression;
struct FormalParameterDeclaration;
struct Statement;

using Declarations = llvm::SmallVector<Declaration*>;
using Expressions = llvm::SmallVector<Expression*>;
using FormalParameterDeclarations = llvm::SmallVector<FormalParameterDeclaration*>;
using Statements = llvm::SmallVector<Statement*>;



/*

	declarations

*/

struct Declaration {
	enum Kind {
		kModule,
		kConstant,
		kType,
		kVariable,
		kParameter,
		kProcedure
		};
	
	
	const Kind	fKind;
	Declaration	*const fEnclosing;
	llvm::SMLoc	fLocation;
	llvm::StringRef	fName;
	
	
			Declaration(
				Kind		kind,
				Declaration	*enclosing,
				llvm::SMLoc	location,
				llvm::StringRef	name
				) :
				fKind(kind),
				fEnclosing(enclosing),
				fLocation(location),
				fName(name)
				{}
	};


struct ModuleDeclaration : public Declaration {
	static bool	classof(const Declaration *d) { return d->fKind == kModule; }
	
	
	Declarations	fDeclarations;
	Statements	fStatements;
	
	
			ModuleDeclaration(
				Declaration	*enclosing,
				llvm::SMLoc	location,
				llvm::StringRef	name
				) :
				Declaration(kModule, enclosing, location, name)
				{}
	};


struct ConstantDeclaration : public Declaration {
	static bool	classof(const Declaration *d) { return d->fKind == kConstant; }
	
	
	Expression	*fExpression;
	
			ConstantDeclaration(
				Declaration	*enclosing,
				llvm::SMLoc	location,
				llvm::StringRef	name,
				Expression	*expression
				) :
				Declaration(kConstant, enclosing, location, name),
				fExpression(expression)
				{} 
	};


struct TypeDeclaration : public Declaration {
	static bool	classof(const Declaration *d) { return d->fKind == kType; }
	
	
			TypeDeclaration(
				Declaration *enclosing,
				llvm::SMLoc location,
				llvm::StringRef name
				) :
				Declaration(kType, enclosing, location, name)
				{}
	};


struct VariableDeclaration : public Declaration {
	static bool	classof(const Declaration *d) { return d->fKind == kVariable; }
	
	
	TypeDeclaration	*fType;
	
			VariableDeclaration(
				Declaration *enclosing,
				llvm::SMLoc location,
				llvm::StringRef name,
				TypeDeclaration *type
				) :
				Declaration(kVariable, enclosing, location, name),
				fType(type)
				{}
	};


struct FormalParameterDeclaration : public Declaration {
	static bool	classof(const Declaration *d) { return d->fKind == kParameter; }
	
	
	TypeDeclaration	*fType;
	bool		fIsVariable;
	
			FormalParameterDeclaration(
				Declaration *enclosing,
				llvm::SMLoc location,
				llvm::StringRef name,
				TypeDeclaration *type,
				bool isVariable
				) :
				Declaration(kParameter, enclosing, location, name),
				fType(type),
				fIsVariable(isVariable)
				{}
	};


struct ProcedureDeclaration : public Declaration {
	static bool	classof(const Declaration *d) { return d->fKind == kProcedure; }
	
	
	FormalParameterDeclarations fParameters;
	TypeDeclaration	*fReturnType;
	Declarations	fDeclarations;
	Statements	fStatements;
	
			ProcedureDeclaration(
				Declaration *enclosing,
				llvm::SMLoc location,
				llvm::StringRef name
				) :
				Declaration(kProcedure, enclosing, location, name)
				{}
	};



/*

	expressions

*/

struct OperatorInfo {
	llvm::SMLoc	fLocation;
	Token::Kind	fKind;
	bool		fIsUnspecified;
	
	
			OperatorInfo() :
				fKind(Token::kUnknown),
				fIsUnspecified(true)
				{}
			
			OperatorInfo(
				llvm::SMLoc location,
				Token::Kind kind,
				bool isUnspecified = false
				) :
				fLocation(location),
				fKind(kind),
				fIsUnspecified(isUnspecified)
				{}
	};


struct Expression {
	enum Kind {
		kInfix,
		kPrefix,
		kInteger,
		kBoolean,
		kVariable,
		kParameter,
		kConstant,
		kFunction,
		};
	
	
	const Kind	fKind;
	TypeDeclaration	*fType;
	bool		fIsConstant;
	
			Expression(
				Kind kind,
				TypeDeclaration *type,
				bool isConstant
				) :
				fKind(kind),
				fType(type),
				fIsConstant(isConstant)
				{}
	};


struct InfixExpression : public Expression {
	static bool	classof(const Expression *e) { return e->fKind == kInfix; }
	
	
	Expression	*fLeft,
			*fRight;
	OperatorInfo	fOperator;

			InfixExpression(
				Expression *left,
				Expression *right,
				OperatorInfo op,
				TypeDeclaration *type,
				bool isConstant
				) :
				Expression(kInfix, type, isConstant),
				fLeft(left),
				fRight(right),
				fOperator(op)
				{}
	};


struct PrefixExpression : public Expression {
	static bool	classof(const Expression *e) { return e->fKind == kPrefix; }
	
	
	Expression	*fExpression;
	OperatorInfo	fOperator;
	
			PrefixExpression(
				Expression *expression,
				OperatorInfo op,
				TypeDeclaration *type,
				bool isConstant
				) :
				Expression(kPrefix, type, isConstant),
				fExpression(expression),
				fOperator(op)
				{}
	};


struct IntegerLiteral : public Expression {
	static bool	classof(const Expression *e) { return e->fKind == kInteger; }
	
	
	// *** why does only IntegerLiteral have a location?
	llvm::SMLoc	fLocation;
	llvm::APSInt	fValue;
	
			IntegerLiteral(
				llvm::SMLoc location,
				const llvm::APSInt &value,
				TypeDeclaration *type
				) :
				Expression(kInteger, type, true),
				fLocation(location),
				fValue(value)
				{}
	};


struct BooleanLiteral : public Expression {
	static bool	classof(const Expression *e) { return e->fKind == kBoolean; }
	
	
	bool		fValue;
	
			BooleanLiteral(
				bool value,
				TypeDeclaration *type
				) :
				Expression(kBoolean, type, true),
				fValue(value)
				{}
	};


struct VariableAccess : public Expression {
	static bool	classof(const Expression *e) { return e->fKind == kVariable; }
	
	
	VariableDeclaration *fVariable;
	
			VariableAccess(
				VariableDeclaration *variable
				) :
				Expression(kVariable, variable->fType, false),
				fVariable(variable)
				{}
	};


struct FormalParameterAccess : public Expression {
	static bool	classof(const Expression *e) { return e->fKind == kParameter; }
	
	
	FormalParameterDeclaration *fParameter;
	
			FormalParameterAccess(
				FormalParameterDeclaration *parameter
				) :
				Expression(kParameter, parameter->fType, false),
				fParameter(parameter)
				{}
	};


struct ConstantAccess : public Expression {
	static bool	classof(const Expression *e) { return e->fKind == kConstant; }
	
	
	ConstantDeclaration *fConstant;
	
			ConstantAccess(
				ConstantDeclaration* constant
				) :
				Expression(kConstant, constant->fExpression->fType, true),
				fConstant(constant)
				{}
	};


struct FunctionCallExpression : public Expression {
	static bool	classof(const Expression *e) { return e->fKind == kFunction; }
	
	
	ProcedureDeclaration *fProcedure;
	Expressions	fParameters;
	
			FunctionCallExpression(
				ProcedureDeclaration *procedure,
				Expressions	parameters
				) :
				Expression(kFunction, procedure->fReturnType, false),
				fProcedure(procedure),
				fParameters(parameters)
				{}
	};



/*

	statements

*/


struct Statement {
	enum Kind {
		kAssignment,
		kProcedureCall,
		kIf,
		kWhile,
		kReturn
		};
	
	
	Kind		fKind;
	
			Statement(Kind kind) : fKind(kind) {}
	};


struct AssignmentStatement : public Statement {
	Declaration	*fDeclaration;
	Expression	*fExpression;
	
			AssignmentStatement(
				Declaration *declaration,
				Expression *expression
				) :
				Statement(kAssignment),
				fDeclaration(declaration),
				fExpression(expression)
				{}
	};


struct ProcedureCallStatement : public Statement {
	ProcedureDeclaration *fProcedure;
	Expressions	fParameters;
	
			ProcedureCallStatement(
				ProcedureDeclaration *procedure,
				Expressions	&parameters
				) :
				Statement(kProcedureCall),
				fProcedure(procedure),
				fParameters(parameters)
				{}
	};


struct IfStatement : public Statement {
	Expression	*fCondition;
	Statements	fThenStatements,
			fElseStatements;
	
			IfStatement(
				Expression	*condition,
				Statements	ifStatements,
				Statements	elseStatements
				) :
				Statement(kIf),
				fCondition(condition),
				fThenStatements(ifStatements),
				fElseStatements(elseStatements)
				{}
	};


struct WhileStatement : public Statement {
	Expression	*fCondition;
	Statements	fStatements;
	
			WhileStatement(
				Expression *condition,
				Statements	&statements
				) :
				Statement(kWhile),
				fCondition(condition),
				fStatements(statements)
				{}
	};


struct ReturnStatement : public Statement {
	Expression	*fReturnValue;
	
			ReturnStatement(Expression *returnValue) :
				Statement(kReturn),
				fReturnValue(returnValue)
				{}
	};
