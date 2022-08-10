#include <llvm/IR/PassManager.h>


class CountIRPass : public llvm::PassInfoMixin<CountIRPass> {
public:
	llvm::PreservedAnalyses run(llvm::Function&, llvm::FunctionAnalysisManager&);
	};
