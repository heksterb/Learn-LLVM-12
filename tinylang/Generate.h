#pragma once

#include <memory>
#include <filesystem>
#include <string>

#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Target/TargetMachine.h>

#include "AST.h"


/*	Generator
	Code generator
	
	It's *possible* that an instance of this class could be used more than once;
	that's why the code generation is exposed as a member function.
*/
struct Generator {
	struct Module;

protected:
	// arguments to conversion
	llvm::LLVMContext &fContext;
	
	// state of conversion
	llvm::Type	*const fTypeVoid,
			*const fTypeInteger1,
			*const fTypeInteger32,
			*const fTypeInteger64;
	llvm::Constant	*const fInteger32Zero;
	
	
	llvm::Type	*mapType(const TypeDeclaration&),
			*mapType(const Declaration&);
	std::string	mangleName(const Declaration&);

public:
			Generator(llvm::LLVMContext&);
	
	std::unique_ptr<llvm::Module> operator()(
				llvm::TargetMachine&,
				const std::filesystem::path&,
				const ModuleDeclaration&
				);
	};


/*	Module
	Module code generator
	
	This class holds the context and temporary state needed to code-generate
	a module.  Since the temporary state cannot be reused between module
	generations, a class-static invoker function is designed to accept an
	rvalue instance.
*/
struct Generator::Module {
	struct Procedure;

protected:
	// arguments to conversion
	Generator	&fGenerator;
	llvm::TargetMachine &fTargetMachine; 
	const ModuleDeclaration &fDeclaration;
	
	// state of conversion
	llvm::DenseMap<const Declaration*, llvm::GlobalObject*> fGlobals;
	
	
	void		emit(const VariableDeclaration&, llvm::Module&),
			emit(const ProcedureDeclaration&, llvm::Module&),
			emit(const Declaration&, llvm::Module&);
	
	std::unique_ptr<llvm::Module> operator()(const std::filesystem::path&);

public:
	static std::unique_ptr<llvm::Module> Generate(Module&&, const std::filesystem::path&);
	
	
			Module(
				Generator	&generator,
				llvm::TargetMachine &targetMachine,
				const ModuleDeclaration &declaration
				) :
				fGenerator(generator),
				fTargetMachine(targetMachine),
				fDeclaration(declaration)
				{}
	};


/*	Procedure
	Module procedure code generator
	
	This class holds the context and temporary state needed to code-generate
	a procedure.  Since the temporary state cannot be reused between module
	generations, a class-static invoker function is designed to accept an
	rvalue instance.
*/
struct Generator::Module::Procedure {
	struct Block;

protected:
	// arguments to conversion
	Module		&fModule;
	Generator	&fGenerator;
	const ProcedureDeclaration &fDeclaration;
	
	// state of conversion
	llvm::IRBuilder<> fBuilder;
	llvm::DenseMap<llvm::BasicBlock*, Block> fDefinitions;
	llvm::DenseMap<FormalParameterDeclaration*, llvm::Argument*> fFormalParameters;
	llvm::Function	*fFunction;
	
	
	static llvm::FunctionType *createFunctionType(Generator&, const ProcedureDeclaration&);
	static llvm::Function *createFunction(Generator&, const ProcedureDeclaration&, llvm::Module&);
	
	
	// basic blocks
	template <typename ...Args>
	Block		&createBlock(Args&&...);
	Block		*BlockOf(llvm::BasicBlock*);
	Block		&Current();
	
	// variable access
	llvm::Value	*readVariable(const VariableDeclaration&),
			*readVariable(const FormalParameterDeclaration&),
			*readVariable(const Declaration&);
	void		writeVariable(const VariableDeclaration&, llvm::Value*),
			writeVariable(const FormalParameterDeclaration&, llvm::Value*),
			writeVariable(const Declaration&, llvm::Value*);
	
	// expressions
	llvm::Value	*emit(const InfixExpression&),
			*emit(const PrefixExpression&),
			*emit(const Expression&);
	
	// statements
	void		emit(const AssignmentStatement&),
			emit(const ProcedureCallStatement&),
			emit(const IfStatement&),
			emit(const WhileStatement&),
			emit(const ReturnStatement&),
			emit(const Statement&),
			emit(const Statements&);
	
	void		operator()();

public:
	static void	Generate(Procedure&&);
	
	
	explicit	Procedure(Module&, const ProcedureDeclaration&, llvm::Module&);
	};


/*	Block
	Basic block under construction
*/
struct Generator::Module::Procedure::Block {
//protected:
	Procedure	&fProcedure;
	llvm::BasicBlock *fBlock;
	
	// maps the variable or formal parameter to its value definition
	llvm::DenseMap<const Declaration*, llvm::TrackingVH<llvm::Value>> fDefinitions;
	
	// incompleted phi instructions
	llvm::DenseMap<llvm::PHINode*, const Declaration*> fIncompletePhis;
	
	bool		fSealed;

public:
			Block(
				Procedure	&procedure,
				llvm::BasicBlock *block
				) :
				fProcedure(procedure),
				fBlock(block),
				fSealed(false)
				{}
	
	operator	llvm::BasicBlock*() { return fBlock; }
	llvm::BasicBlock *operator->() { return fBlock; }
	
	void		addPhiOperands(const Declaration&, llvm::PHINode*);
	llvm::PHINode	*addEmptyPhi(const Declaration&);
	void		optimizePhi(llvm::PHINode*);
	llvm::Value	*read(const Declaration&);
	void		write(const Declaration&, llvm::Value*);
	void		seal();
	};


/*	Generator::Module::Procedure::Generate
	Emit code for the given procedure; guarantees that it can only be done once
*/
inline void Generator::Module::Procedure::Generate(
	Procedure	&&procedure
	) {
	procedure();
	}


/*	BlockOf
	Find the block object corresponding to the given LLVM block
*/
inline Generator::Module::Procedure::Block *Generator::Module::Procedure::BlockOf(
	llvm::BasicBlock *block
	)
{
return
	block ?
		&fDefinitions.find(block)->second :
		nullptr;
}


/*	Current
	Return the block object corresponding to the builder's current
*/
inline Generator::Module::Procedure::Block &Generator::Module::Procedure::Current()
{
return fDefinitions.find(fBuilder.GetInsertBlock())->second;
}


/*	Generator::Module::Generate
	Emit code for the given module; guarantees that it can only be done once
*/
inline std::unique_ptr<llvm::Module> Generator::Module::Generate(
	Module		&&module,
	const std::filesystem::path &file
	) {
	return module(file);
	}


/*	Generator::()
	Convenience function to emit code for a module
*/
inline std::unique_ptr<llvm::Module> Generator::operator()(
	llvm::TargetMachine &machine,
	const std::filesystem::path &file,
	const ModuleDeclaration &moduleDeclaration
	) {
	return Module::Generate(Module(*this, machine, moduleDeclaration), file);
	}
