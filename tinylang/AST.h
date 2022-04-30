#pragma once

#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>

#include "Tokens.h"


struct Declaration;
struct Expression;
struct ParameterDeclaration;
struct Statement;

using Declarations = llvm::SmallVector<Declaration*>;
using Expressions = llvm::SmallVector<Expression*>;
using ParameterDeclarations = llvm::SmallVector<ParameterDeclaration*>;
using Statements = llvm::SmallVector<Statement*>;



/*

	declarations

*/

struct Declaration {
	enum Kind {
		kModule,
		kConstant,
		kTypeAlias,
		kTypeArray,
		kTypePervasive,
		kTypePointer,
		kTypeRecord,
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
	static bool	classof(const Declaration *d) {
				return
					d->fKind >= kTypeAlias &&
					d->fKind <= kTypeRecord;
				}
	
	
			TypeDeclaration(
				Kind		kind,
				Declaration	*enclosing,
				llvm::SMLoc	location,
				llvm::StringRef	name
				) :
				Declaration(kind, enclosing, location, name)
				{}
	};


struct AliasTypeDeclaration : public TypeDeclaration {
	static bool	classof(const Declaration *d) { return d->fKind == kTypeAlias; }
	
	
	TypeDeclaration	*fType;
	
			AliasTypeDeclaration(
				Declaration	*enclosing,
				llvm::SMLoc	location,
				llvm::StringRef	name,
				TypeDeclaration	*type
				) :
				TypeDeclaration(kTypeAlias, enclosing, location, name),
				fType(type)
				{}
	};


struct ArrayTypeDeclaration : public TypeDeclaration {
	static bool	classof(const Declaration *d) { return d->fKind == kTypeArray; }
	
	
	TypeDeclaration	*fType;
	Expression	*fCount;
	
			ArrayTypeDeclaration(
				Declaration	*enclosing,
				llvm::SMLoc	location,
				llvm::StringRef	name,
				TypeDeclaration	*type,
				Expression	*count
				) :
				TypeDeclaration(kTypeArray, enclosing, location, name),
				fType(type),
				fCount(count)
				{}
	};


struct PervasiveTypeDeclaration : public TypeDeclaration {
	static bool	classof(const Declaration *d) { return d->fKind == kTypePervasive; }
	
	
			PervasiveTypeDeclaration(
				Declaration	*enclosing,
				llvm::SMLoc	location,
				llvm::StringRef	name
				) :
				TypeDeclaration(kTypePervasive, enclosing, location, name)
				{}
	};


struct PointerTypeDeclaration : public TypeDeclaration {
	static bool	classof(const Declaration *d) { return d->fKind == kTypePointer; }
	
	
	TypeDeclaration	*fType;
	
			PointerTypeDeclaration(
				Declaration	*enclosing,
				llvm::SMLoc	location,
				llvm::StringRef	name,
				TypeDeclaration	*type
				) :
				TypeDeclaration(kTypePointer, enclosing, location, name),
				fType(type)
				{}
	};


struct RecordTypeDeclaration : public TypeDeclaration {
	static bool	classof(const Declaration *d) { return d->fKind == kTypeRecord; }
	
	
	struct Field {
		llvm::SMLoc	fLocation;
		llvm::StringRef	fName;
		TypeDeclaration	*fType;
		};
	
	using Fields = std::vector<Field>;
	
	
	Fields		fFields;
	
			RecordTypeDeclaration(
				Declaration	*enclosing,
				llvm::SMLoc	location,
				llvm::StringRef	name,
				std::vector<Field> &&fields
				) :
				TypeDeclaration(kTypeRecord, enclosing, location, name),
				fFields(std::move(fields))
				{}
	};


struct NameDeclaration : public Declaration {
	TypeDeclaration	*fType;
	
			NameDeclaration(
				Kind		kind,
				Declaration	*enclosing,
				llvm::SMLoc	location,
				llvm::StringRef	name,
				TypeDeclaration	*type
				) :
				Declaration(kind, enclosing, location, name),
				fType(type)
				{}
	};


struct VariableDeclaration : public NameDeclaration {
	static bool	classof(const Declaration *d) { return d->fKind == kVariable; }
	
	
			VariableDeclaration(
				Declaration	*enclosing,
				llvm::SMLoc	location,
				llvm::StringRef	name,
				TypeDeclaration	*type
				) :
				NameDeclaration(kVariable, enclosing, location, name, type)
				{}
	};


struct ParameterDeclaration : public NameDeclaration {
	static bool	classof(const Declaration *d) { return d->fKind == kParameter; }
	
	
	bool		fIsVariable;
	
			ParameterDeclaration(
				Declaration *enclosing,
				llvm::SMLoc location,
				llvm::StringRef name,
				TypeDeclaration *type,
				bool isVariable
				) :
				NameDeclaration(kParameter, enclosing, location, name, type),
				fIsVariable(isVariable)
				{}
	};


struct ProcedureDeclaration : public Declaration {
	static bool	classof(const Declaration *d) { return d->fKind == kProcedure; }
	
	
	ParameterDeclarations fParameters;
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

	designator selectors

*/

struct Selector {
	enum Kind {
		kIndex,
		kField,
		kDereference
		};
	
	Kind		fKind;
	TypeDeclaration	*fType;			// type of the selector expression
	
			Selector(
				Kind		kind,
				TypeDeclaration	*type
				) :
				fKind(kind),
				fType(type)
				{}
	};


struct IndexSelector : public Selector {
	static bool	classof(const Selector *s) { return s->fKind == kIndex; }
	
	
	Expression	*fIndex;
	
			IndexSelector(
				TypeDeclaration	*type,
				Expression 	*index
				) :
				Selector(kIndex, type),
				fIndex(index)
				{}
	};


struct FieldSelector : public Selector {
	static bool	classof(const Selector *s) { return s->fKind == kField; }
	
	
	uint32_t	fIndex;
	llvm::StringRef	fName;
	
			FieldSelector(
				TypeDeclaration	*type,
				uint32_t	index,
				llvm::StringRef	name
				) :
				Selector(kField, type),
				fIndex(index),
				fName(name)
				{}
	};


struct DereferenceSelector : public Selector {
	static bool	classof(const Selector *s) { return s->fKind == kDereference; }
	
	
			DereferenceSelector(
				TypeDeclaration	*type
				) :
				Selector(kDereference, type)
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
		kAccess,
		kDesignator,
		kParameter,
		kConstant,
		kFunction,
		};
	
	
	const Kind	fKind;
	TypeDeclaration	*fType;
	bool		fIsConstant;
	
			Expression(
				Kind		kind,
				TypeDeclaration	*type,
				bool		isConstant
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


struct LeftValue : public Expression {
	using		Expression::Expression;
	};


struct Access : public LeftValue {
	static bool	classof(const Expression *e) { return e->fKind == kAccess; }
	
	
	NameDeclaration	*fVariable;
	
			Access(
				NameDeclaration *variable
				) :
				LeftValue(kAccess, variable->fType, false),
				fVariable(variable)
				{}
	};


struct Designator : public LeftValue {
	static bool	classof(const Expression *e) { return e->fKind == kDesignator; }
	
	
	LeftValue	*fLeftValue;
	Selector	*fSelector;
	
			Designator(
				LeftValue	*leftValue,
				Selector	*selector
				) :
				LeftValue(kDesignator, selector->fType, false),
				fLeftValue(leftValue),
				fSelector(selector)
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
	LeftValue	*fLeftValue;
	Expression	*fExpression;
	
			AssignmentStatement(
				LeftValue	*leftValue,
				Expression	*expression
				) :
				Statement(kAssignment),
				fLeftValue(leftValue),
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
