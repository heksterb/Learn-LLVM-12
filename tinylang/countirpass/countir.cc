#include <llvm/ADT/Statistic.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>
#include <llvm/Support/Debug.h>

#include "countir.h"


#define DEBUG_TYPE "countir"


STATISTIC(numberOfInstructions, "Number of instructions");
STATISTIC(numberOfBasicBlocks, "Number of basic blocks");


/*	run
	Run the pass
*/
llvm::PreservedAnalyses CountIRPass::run(
	llvm::Function	&f,
	llvm::FunctionAnalysisManager &analysisManager
	)
{
for (llvm::BasicBlock &block: f) {
	numberOfBasicBlocks++;
	
	for (llvm::Instruction &i: block)
		numberOfInstructions++;
	}

return llvm::PreservedAnalyses::all();
}


/*	pipelineParser
	Invoked at run-time to potentially add the pass
*/
static bool pipelineParser(
	llvm::StringRef	name,
	llvm::FunctionPassManager &passManager,
	llvm::ArrayRef<llvm::PassBuilder::PipelineElement>
	)
{
bool result;

if (name =="countir")  {
	/* temporary moved through template member function addPass() */
	passManager.addPass(CountIRPass());
	
	result = true;
	}

else
	result = false;

return result;
}


/*	PassPluginLibraryInfo
	Entry point
*/
extern "C" llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK llvmGetPassPluginInfo()
{
return llvm::PassPluginLibraryInfo {
	LLVM_PLUGIN_API_VERSION,
	"CountIR", "v0.1",
	
	// invoked for the plug-in to register itself
	[](llvm::PassBuilder &builder) {
		builder.registerPipelineParsingCallback(&pipelineParser);
		}
	};
}

