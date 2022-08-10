#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/SymbolStringPool.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>

#include "jit.h"


/*	optimizeModuleThreadSafe

*/
void JIT::optimizeModuleThreadSafe(
	llvm::Module	&module
	)
{
bool debug = false;

llvm::PassBuilder passBuilder(debug);
llvm::LoopAnalysisManager loopAnalysis(debug);
llvm::FunctionAnalysisManager functionAnalysis(debug);
llvm::CGSCCAnalysisManager cgsccAnalysis(debug);
llvm::ModuleAnalysisManager moduleAnalysis(debug);

functionAnalysis.registerPass(
	[&passBuilder] { return passBuilder.buildDefaultAAPipeline(); }
	);

/* The order of these turns out to be important: reorganize them to line up
   with the declaration order, and you get malloc() errors. */
passBuilder.registerModuleAnalyses(moduleAnalysis);
passBuilder.registerCGSCCAnalyses(cgsccAnalysis);
passBuilder.registerFunctionAnalyses(functionAnalysis);
passBuilder.registerLoopAnalyses(loopAnalysis);
passBuilder.crossRegisterProxies(loopAnalysis, functionAnalysis, cgsccAnalysis, moduleAnalysis);

llvm::ModulePassManager modulePassManager = passBuilder.buildPerModuleDefaultPipeline(
	llvm::PassBuilder::OptimizationLevel::O2,
	debug
	);

modulePassManager.run(module, moduleAnalysis);
}


/*	optimizeModule

*/
llvm::Expected<llvm::orc::ThreadSafeModule> JIT::optimizeModule(
	llvm::orc::ThreadSafeModule tsm,
	const llvm::orc::MaterializationResponsibility &r
	)
{
tsm.withModuleDo(optimizeModuleThreadSafe);

return std::move(tsm);
}


/*	JIT
	Prepare to do just-in-time compilation
*/
JIT::JIT() :
	fStringPool(std::make_shared<llvm::orc::SymbolStringPool>()),
	fTargetProcessControl(
		/* honestly I would prefer to throw an exception here; but I've already
		   conceded to the 'LLVM way' in a number of ways already */
		llvm::cantFail(
			llvm::orc::SelfTargetProcessControl::Create(fStringPool)
			)
		),
	fTargetMachineBuilder(fTargetProcessControl->getTargetTriple()),
	fDataLayout(
		llvm::cantFail(
			fTargetMachineBuilder.getDefaultDataLayoutForTarget()
			)
		),
	fExecutionSession(fStringPool),
	fMangleAndInterner(fExecutionSession, fDataLayout),
	fLinkLayer(
		fExecutionSession,
		[] { return std::make_unique<llvm::SectionMemoryManager>(); }
		),
	fCompileLayer(
		fExecutionSession,
		fLinkLayer,
		std::make_unique<llvm::orc::ConcurrentIRCompiler>(
			std::move(fTargetMachineBuilder)
			)
		),
	fTransformLayer(fExecutionSession, fCompileLayer, optimizeModule),
	fMainJITDylib(fExecutionSession.createBareJITDylib("<main>"))
{
// represent symbols in current executable
fMainJITDylib.addGenerator(
	llvm::cantFail(
		llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
			fDataLayout.getGlobalPrefix()
			)
		)
	);
}


/*	~JIT
	
*/
JIT::~JIT()
{
/* This is not in the book, but is in the book's downloaded code.  It's necessary to
   prevent all sorts of 'destructing with resources still attached' errors. */
if (llvm::Error error = fExecutionSession.endSession())
	fExecutionSession.reportError(std::move(error));
}


/*	addModule
	Add the given LLVM Module to be just-in-time compiled
*/
void JIT::addModule(
	llvm::orc::ThreadSafeModule module,
	llvm::orc::ResourceTrackerSP resourceTracker
	)
{
if (!resourceTracker)
	resourceTracker = fMainJITDylib.getDefaultResourceTracker();

if (llvm::Error error = fTransformLayer.add(resourceTracker, std::move(module)))
	throw error;
}


/*	lookUp
	'Look up' the given symbol by compiling it and returning a pointer to the result
*/
llvm::JITEvaluatedSymbol JIT::lookUp(
	llvm::StringRef	name
	)
{
llvm::Expected<llvm::JITEvaluatedSymbol> symbolp = fExecutionSession.lookup(
	{ &fMainJITDylib },
	fMangleAndInterner(name.str())
	); if (!symbolp) throw symbolp.takeError();

return *symbolp;
}



/*

	command-line options

*/

static llvm::cl::opt<std::string> gInputFile(llvm::cl::Positional, llvm::cl::Required, llvm::cl::desc("<input-file>"));


/*	loadModule

*/
static std::unique_ptr<llvm::Module> loadModule(
	llvm::StringRef	fileName,
	llvm::LLVMContext &context,
	const char	*const programName
	)
{
llvm::SMDiagnostic diagnostic;
std::unique_ptr<llvm::Module> module = llvm::parseIRFile(fileName, diagnostic, context);
if (!module) throw "can't load module";

return module;
}


/*	main
	Command-line entry point
*/
int main(
	int		argc,
	char		*argv[]
	)
{
int result = 0;

// initialize LLVM
llvm::InitLLVM llvm(argc, argv);

llvm::InitializeNativeTarget();
llvm::InitializeNativeTargetAsmPrinter();
llvm::InitializeNativeTargetAsmParser();

llvm::cl::ParseCommandLineOptions(argc, argv);

try {
	// just-in-time compiler
	JIT jit;
	
	// add LLVM IR module to LLJIT
	std::unique_ptr<llvm::LLVMContext> context = std::make_unique<llvm::LLVMContext>();
	std::unique_ptr<llvm::Module> module = loadModule(gInputFile, *context, argv[0]);
	jit.addModule(
		llvm::orc::ThreadSafeModule(std::move(module), std::move(context))
		);
	
	// look up 'main' function
	llvm::JITEvaluatedSymbol mainSymbol = jit.lookUp("main");
	
	// find address of 'main' function
	int (*const mainFunction)(int, char**) = reinterpret_cast<int (*)(int, char**)>(mainSymbol.getAddress());
	
	// invoke it
	(void) (*mainFunction)(argc, argv);
	}

catch (llvm::Error &error) {
	llvm::WithColor::error(llvm::errs(), argv[0]) << error << '\n';
	
	result = 1;
	}

catch (const char error[]) {
	llvm::WithColor::error(llvm::errs(), argv[0]) << error << '\n';
	
	result = 1;
	}

catch (...) {
	llvm::WithColor::error(llvm::errs(), argv[0]) << "error\n";
	
	result = 1;
	}

return result;
}

