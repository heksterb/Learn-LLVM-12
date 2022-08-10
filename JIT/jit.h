#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/Mangling.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/TargetProcessControl.h>
#include <llvm/IR/DataLayout.h>


struct JIT {
protected:
	static void optimizeModuleThreadSafe(llvm::Module&);
	static llvm::Expected<llvm::orc::ThreadSafeModule> optimizeModule(
		llvm::orc::ThreadSafeModule,
		const llvm::orc::MaterializationResponsibility&
		);
	
	std::shared_ptr<llvm::orc::SymbolStringPool> fStringPool;
	std::unique_ptr<llvm::orc::SelfTargetProcessControl> fTargetProcessControl;
	llvm::orc::JITTargetMachineBuilder fTargetMachineBuilder;
	llvm::DataLayout	fDataLayout;
	llvm::orc::ExecutionSession fExecutionSession;
	llvm::orc::MangleAndInterner fMangleAndInterner;
	llvm::orc::RTDyldObjectLinkingLayer fLinkLayer;
	llvm::orc::IRCompileLayer fCompileLayer;
	llvm::orc::IRTransformLayer fTransformLayer;
	llvm::orc::JITDylib	&fMainJITDylib;

public:
			JIT();
			~JIT();
	
	void		addModule(llvm::orc::ThreadSafeModule, llvm::orc::ResourceTrackerSP = nullptr);
	llvm::JITEvaluatedSymbol lookUp(llvm::StringRef);
	};
