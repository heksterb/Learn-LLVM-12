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
	const NameDeclaration &declaration
	)
{
llvm::Type *const type = fProcedure.fGenerator.mapType(declaration);

return
	fBlock->empty() ?
		llvm::PHINode::Create(type, 0, "", fBlock) :
		llvm::PHINode::Create(type, 0, "", &fBlock->front());
}


/*	optimizePhi
	Try to replace the given 'phi' instruction with a single common incoming value
	
	Return the common incoming value, which may just be the 'phi' instruction itself.
*/
llvm::Value *Generator::Module::Procedure::Block::optimizePhi(
	llvm::PHINode	*phi
	)
{
/* Figure whether all the incoming values of the 'phi' instruction are the same
   Incoming values that refer to the 'phi' itself are disregarded.
   *** under what circumstances can the incoming value of the phi be the phi itself?!
*/
llvm::Value *same = nullptr;
for (llvm::Value *value: phi->incoming_values())
	// this incoming value is still the same as the preceding ones?
	if (value == same)
		;
	
	// this incoming value is the 'phi' instruction itself?
	else if (value == phi)
		;
	
	// there were preceding incoming values
	else if (same) {
		// but this incoming value is different?
		if (value != same)
			// no optimization possible
			return phi;
		}
	
	// haven't seen any preceding incoming values yet?
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

return same;
}


/*	addPhiOperands
	Add predecessor operands to the given incomplete 'phi' node
	
	Return the common incoming value after potential optimization
*/
llvm::Value *Generator::Module::Procedure::Block::addPhiOperands(
	const NameDeclaration &declaration,
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

return optimizePhi(phi);
}


/*	seal
	Mark that all our predecessors are now known
*/
void Generator::Module::Procedure::Block::seal()
{
assert(!fSealed);

// complete all the yet-incomplete PHI nodes
for (const std::pair<llvm::PHINode*, const NameDeclaration*> &phi: fIncompletePhis)
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
	const NameDeclaration &declaration
	)
{
llvm::Value *result;

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
	
	// multiple predecessors
	else {
		// create empty 'phi' instruction to break potential cycles
		llvm::PHINode *const phi = addEmptyPhi(declaration);
		result = addPhiOperands(declaration, phi);
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
	const NameDeclaration &declaration,
	llvm::Value	*value
	)
{
assert(llvm::isa<NameDeclaration>(declaration));

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
const ParameterDeclarations &parameters = declaration.fParameters;
std::transform(
	std::begin(parameters), std::end(parameters),
	std::back_inserter(parameterTypes),
	[&generator](ParameterDeclaration *parameter) { return generator.mapType(*parameter); }
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
	ParameterDeclaration &parameterDeclaration = *declaration.fParameters[argumentI];
	
	if (parameterDeclaration.fIsVariable) {
		llvm::AttrBuilder attributes;
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
	fGenerator(module.fGenerator),
	fDeclaration(declaration),
	fModule(module),
	fBuilder(fGenerator.fContext),
	fFunction(createFunction(fGenerator, declaration, result))
{
}


/*	createBlock
	Create a basic block
*/
Generator::Module::Procedure::Block &Generator::Module::Procedure::createBlock(
	const char	name[]
	)
{
// create the LLVM block
llvm::BasicBlock *block = llvm::BasicBlock::Create(fGenerator.fContext, name, fFunction);

// now create the block object in our map
return fDefinitions.try_emplace(block, *this, block).first->second;
}


/*	readVariable
	Return the LLVM value corresponding to the given variable
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
	result = fModule.fGlobals.find(&declaration)->second;

else
	throw "unsupported: nested procedures";

return result;
}


/*	readVariable
	Emit code to read a value from a given parameter
*/
llvm::Value *Generator::Module::Procedure::readVariable(
	const ParameterDeclaration &declaration
	)
{
return
	declaration.fIsVariable ?
		fParameters.find(&declaration)->second :
		Current().read(declaration);
}


/*	readVariable
	Emit code to read a value from a given declaration
*/
llvm::Value *Generator::Module::Procedure::readVariable(
	const NameDeclaration &declaration
	)
{
llvm::Value *result;

switch (declaration.fKind) {
	case Declaration::kVariable:
		result = readVariable(llvm::cast<VariableDeclaration>(declaration));
		break;

	case Declaration::kParameter:
		result = readVariable(llvm::cast<ParameterDeclaration>(declaration));
		break;
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
	const ParameterDeclaration &declaration,
	llvm::Value	*value
	)
{
if (declaration.fIsVariable)
	fBuilder.CreateStore(
		value,
		fParameters.find(&declaration)->second
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
		writeVariable(llvm::cast<ParameterDeclaration>(declaration), value);
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


llvm::Value *Generator::Module::Procedure::emitGEP(
	const Designator &designator
	)
{
llvm::Value *result;

if (designator.fSelector->fKind == Selector::kDereference) {
	result = emit(*designator.fLeftValue);
	result = fBuilder.CreateLoad(result->getType()->getPointerElementType(), result);
	}

else {
	llvm::SmallVector<llvm::Value*, 4> indexes;
	
	/* Not entirely sure what this does: why we need this first zero index.  Without it, the eventual
	   type is pointer to 'struct' instead of pointer to the field type (integer).  So it leaves
	   somehow an extra undereferenced pointer. */
	indexes.push_back(fGenerator.fInteger32Zero);
	
	// find contiguous range of (field or array) accessor selectors
	const Expression *e;
	const Designator *d = &designator;
	do {
		switch (const Selector *s = d->fSelector; s->fKind) {
			case Selector::kIndex: {
				const IndexSelector &index = *static_cast<const IndexSelector*>(s);
				
				indexes.push_back(emit(*index.fIndex));
				}
				break;
			
			case Selector::kField: {
				const FieldSelector &field = *static_cast<const FieldSelector*>(s);
				
				indexes.push_back(
					llvm::ConstantInt::get(
						fGenerator.fTypeInteger32, field.fIndex
						)
					);
				}
				break;
			}
		
		e = d->fLeftValue;
		}
		while (
			(d = llvm::dyn_cast<Designator>(e)) && d->fSelector &&
			(d->fSelector->fKind == Selector::kIndex || d->fSelector->fKind == Selector::kField)
			);

	result = emit(*e);
	result = fBuilder.CreateInBoundsGEP(result, indexes);
	}

return result;
}


/*	emit
	Emit code for a variable access expression
*/
llvm::Value *Generator::Module::Procedure::emit(
	const Access	&access
	)
{
return readVariable(*access.fVariable);
}


/*	emit
	Emit code for a designator expression
*/
llvm::Value *Generator::Module::Procedure::emit(
	const Designator &designator
	)
{
llvm::Value *result;

result = emitGEP(designator);
result = fBuilder.CreateLoad(result->getType()->getPointerElementType(), result);

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
	
	case Expression::kAccess:
		result = emit(static_cast<const Access&>(expression));
		break;
	
	case Expression::kDesignator:
		result = emit(static_cast<const Designator&>(expression));
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

Expression &leftValue = *statement.fLeftValue;

switch (leftValue.fKind) {
	case Expression::kAccess: {
		const Access &access = static_cast<Access&>(leftValue);
		
		writeVariable(*access.fVariable, value);
		}
		break;
	
	case Expression::kDesignator: {
		const Designator &designator = static_cast<Designator&>(leftValue);
		
		llvm::Value *base = emitGEP(designator);
		
		fBuilder.CreateStore(value, base);
		}
		break;
	}
	// *** why 32-bit integers here?
	// *** why dereference not implemented here?
	// *** why contiguous selector kinds not implemented here?
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
	&blockIf = createBlock("if.body"),
	*blockElse =
		!statement.fElseStatements.empty() ?
			&createBlock("else.body") :
			nullptr,
	&blockAfter = createBlock("after.if");

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
	&blockCondition = createBlock("while.cond"),
	&blockBody = createBlock("while.body"),
	&blockAfter = createBlock("after.while");

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
Block &block = createBlock("entry");
fBuilder.SetInsertPoint(block);

// parallel iteration over parameters/arguments
for (unsigned argumentI = 0; argumentI < fFunction->arg_size(); argumentI++) {
	llvm::Argument &argument = *fFunction->getArg(argumentI);
	ParameterDeclaration &parameterDeclaration = *fDeclaration.fParameters[argumentI];
	
	// create mapping of Parameter to llvm::Argument for VAR parameters
	fParameters[&parameterDeclaration] = &argument;
	
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
if (!fBuilder.GetInsertBlock()->getTerminator())
	fBuilder.CreateRetVoid();

Current().seal();
}



/*

	module generator

*/

/*	Module
	Prepare to generate code for a module
*/
Generator::Module::Module(
	Generator	&generator,
	llvm::TargetMachine &targetMachine,
	const ModuleDeclaration &declaration,
	const std::filesystem::path &file
	) :
	fGenerator(generator),
	fTargetMachine(targetMachine),
	fDeclaration(declaration),
	
	// create "LLVM module"
	fModule(std::make_unique<llvm::Module>(file.native(), generator.fContext))
{
fModule->setTargetTriple(fTargetMachine.getTargetTriple().getTriple());
fModule->setDataLayout(fTargetMachine.createDataLayout());
}


/*	emit
	Emit code for a module-level ('global') variable declaration
*/
void Generator::Module::emit(
	const VariableDeclaration &declaration
	)
{
llvm::Type *const type = fGenerator.mapType(*declaration.fType);

// emit code to create global variable
(void) fGlobals.try_emplace(
	&declaration,
	new llvm::GlobalVariable(
		*fModule,
		type,
		false /* isConstant */,
		
		/* Actually should just be PrivateLinkage, but make it external so
		   I could make the test verify this. */
		llvm::GlobalValue::ExternalLinkage,
		
		// global variable definition must be initialized, even if with an undefined value
		llvm::UndefValue::get(type),
		
		fGenerator.mangleName(declaration)
		)
	);
}


/*	emit
	Emit code for a procedure declaration
*/
void Generator::Module::emit(
	const ProcedureDeclaration &declaration
	)
{
// emit the procedure
Procedure::Generate(Procedure(*this, declaration, *fModule));
}


/*	emit
	Emit code for the given module-level declaration
*/
void Generator::Module::emit(
	const Declaration &declaration
	)
{
// simulate virtual function dispatch for LLVM-style inheritance
// *** this must be handled carefully to prevent infinite recursion!
switch (declaration.fKind) {
	case Declaration::kVariable:
		emit(llvm::cast<const VariableDeclaration>(declaration));
		break;
	
	case Declaration::kProcedure:
		emit(llvm::cast<const ProcedureDeclaration>(declaration));
		break;
	}
}


/*	aliasType
	Return the TBAA metadata for the record type declaration
*/
llvm::MDNode *Generator::Module::aliasType(
	const RecordTypeDeclaration &declaration
	)
{
llvm::StructType *const type = llvm::cast<llvm::StructType>(fGenerator.mapType(declaration));
const llvm::StructLayout *const layout = fModule->getDataLayout().getStructLayout(type);

// construct the fields list for the TBAA 'struct' node
llvm::SmallVector<std::pair<llvm::MDNode*, uint64_t>> fields;
std::transform(
	std::begin(declaration.fFields), std::end(declaration.fFields),
	std::back_inserter(fields),
	[&](const RecordTypeDeclaration::Field &f) {
		return std::pair<llvm::MDNode*, uint64_t>(
			aliasType(*f.fType),
			layout->getElementOffset(
				// index of field in record
				std::distance(&f, &*std::begin(declaration.fFields))
				)
			);
		}
	);

llvm::StringRef name = fGenerator.mangleName(declaration);

return fGenerator.fMetadataHelper.createTBAAStructTypeNode(name, fields);
}


/*	aliasType
	Return the TBAA metadata for the declaration
*/
llvm::MDNode *Generator::Module::aliasType(
	const TypeDeclaration &declaration
	)
{
llvm::MDNode *result = fAliases[&declaration];

// not cached?
if (!result) {
	switch (declaration.fKind) {
		case Declaration::kTypePervasive:
			result = fGenerator.fMetadataHelper.createTBAAScalarTypeNode(declaration.fName, fGenerator.fAliasRoot);
			break;
		
		case Declaration::kTypePointer:
			result = fGenerator.fMetadataHelper.createTBAAScalarTypeNode("any pointer", fGenerator.fAliasRoot);
			break;
		
		case Declaration::kTypeRecord:
			result = aliasType(static_cast<const RecordTypeDeclaration&>(declaration));
			break;
		}
	
	fAliases[&declaration] = result;
	}

return result;
}


/*	accessTag
	Return the TBAA 'access tag' for the type declaration
	
	This is the 'alias type' metadata node for pointers, and null for everything else
*/
llvm::MDNode *Generator::Module::accessTag(
	const TypeDeclaration	&declaration
	)
{
return
	declaration.fKind == Declaration::kTypePointer ?
		aliasType(declaration) :
		nullptr;
}


/*	()
	Generate module code into the given file
*/
void Generator::Module::operator()()
{
// for each declaration
for (const Declaration *const declaration: fDeclaration.fDeclarations)
	// emit code for the declaration
	emit(*declaration);
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
	fInteger32Zero(llvm::ConstantInt::get(fTypeInteger32, 0, true /* isSigned */)),
	fMetadataHelper(llvm::MDBuilder(context)),
	fAliasRoot(fMetadataHelper.createTBAARoot("Tinylang TBAA"))
{
}


/*	mapType
	Return the LLVM IR type for the given 'pervasive type' declaration
*/
llvm::Type *Generator::mapType(
	const PervasiveTypeDeclaration &declaration
	)
{
return
	declaration.fName == "BOOLEAN" ? fTypeInteger1 :
	declaration.fName == "INTEGER" ? fTypeInteger64 :
	nullptr;
}


/*	mapType
	Return the LLVM IR type for the given alias type declaration
*/
llvm::Type *Generator::mapType(
	const AliasTypeDeclaration &declaration
	)
{
return mapType(*declaration.fType);
}


/*	mapType
	Return the LLVM IR type for the given array type declaration
*/
llvm::Type *Generator::mapType(
	const ArrayTypeDeclaration &declaration
	)
{
llvm::Type *const component = mapType(*declaration.fType);
Expression *const count = declaration.fCount;

return llvm::ArrayType::get(component, 5 /* *** evaluate count */);
}


/*	mapType
	Return the LLVM IR type for the type declaration
*/
llvm::Type *Generator::mapType(
	const RecordTypeDeclaration &declaration
	)
{
llvm::SmallVector<llvm::Type*, 4> fields;
std::transform(
	std::begin(declaration.fFields), std::end(declaration.fFields),
	std::back_inserter(fields),
	[this](const RecordTypeDeclaration::Field &field) {
		return mapType(*field.fType);
		}
	);

return llvm::StructType::create(fields, declaration.fName, false);
}


/*	mapType
	Return the LLVM IR type for the type declaration
*/
llvm::Type *Generator::mapType(
	const TypeDeclaration &declaration
	)
{
llvm::Type *result = fTypes[&declaration];

if (!result) {
	switch (declaration.fKind) {
		case Declaration::kTypePervasive:
			result = mapType(llvm::cast<PervasiveTypeDeclaration>(declaration));
			break;
		
		case Declaration::kTypeAlias:
			result = mapType(llvm::cast<AliasTypeDeclaration>(declaration));
			break;
		
		case Declaration::kTypeArray:
			result = mapType(llvm::cast<ArrayTypeDeclaration>(declaration));
			break;
		
		case Declaration::kTypeRecord:
			result = mapType(llvm::cast<RecordTypeDeclaration>(declaration));
			break;
		}
	
	// cache result
	if (result) fTypes[&declaration] = result;
	}

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
		const ParameterDeclaration &formalParameter = llvm::cast<ParameterDeclaration>(declaration);
		
		type = mapType(*formalParameter.fType);
		if (formalParameter.fIsVariable)
			type = type->getPointerTo();
		}
		break;
	
	case Declaration::kVariable:
		type = mapType(*llvm::cast<VariableDeclaration>(declaration).fType);
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
