#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>


/*

	command-line options

*/

static llvm::cl::opt<std::string> gInputFile(llvm::cl::Positional, llvm::cl::Required, llvm::cl::desc("<input-file>"));


/*	jitmain

*/
static void jitmain(
	std::unique_ptr<llvm::LLVMContext> context,
	std::unique_ptr<llvm::Module> module,
	int		argc,
	char		*argv[]
	)
{
// build LLJIT instance
llvm::Expected<std::unique_ptr<llvm::orc::LLJIT>> jitpp = llvm::orc::LLJITBuilder().create();
if (!jitpp) throw jitpp.takeError();
llvm::orc::LLJIT &jit = **jitpp;

// add LLVM IR module to LLJIT
if (llvm::Error error = jit.addIRModule(
	llvm::orc::ThreadSafeModule(std::move(module), std::move(context))
	)) throw error;;

// represent symbols in current executable
llvm::Expected<std::unique_ptr<llvm::orc::DynamicLibrarySearchGenerator>> dynamicLibrarySearchGeneratorpp = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(jit.getDataLayout().getGlobalPrefix());
if (!dynamicLibrarySearchGeneratorpp) throw dynamicLibrarySearchGeneratorpp.takeError();
std::unique_ptr<llvm::orc::DynamicLibrarySearchGenerator> dynamicLibrarySearchGeneratorp = std::move(*dynamicLibrarySearchGeneratorpp);

// make those symbols available to LLJIT
jit.getMainJITDylib().addGenerator(std::move(dynamicLibrarySearchGeneratorp));

// look up 'main' function
llvm::Expected<llvm::JITEvaluatedSymbol> mainSymbolp = jit.lookup("main");
if (!mainSymbolp) throw mainSymbolp.takeError();
llvm::JITEvaluatedSymbol &mainSymbol = *mainSymbolp;

// find address of 'main' function
int (*mainFunction)(int, char**) = reinterpret_cast<int (*)(int, char**)>(mainSymbol.getAddress());

// invoke it
(void) (*mainFunction)(argc, argv);
}



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
if (!module) {
	diagnostic.print(programName, llvm::errs());
	throw "can't load module";
	}

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

llvm::InitLLVM llvm(argc, argv);

llvm::InitializeNativeTarget();
llvm::InitializeNativeTargetAsmPrinter();
llvm::InitializeNativeTargetAsmParser();

llvm::cl::ParseCommandLineOptions(argc, argv);

std::unique_ptr<llvm::LLVMContext> context = std::make_unique<llvm::LLVMContext>();

std::unique_ptr<llvm::Module> module = loadModule(gInputFile, *context, argv[0]);

llvm::ExitOnError eoe;

try {
	jitmain(std::move(context), std::move(module), argc, argv);
	}

catch (llvm::Error &error) {
	llvm::WithColor::error(llvm::errs(), argv[0]) << error << '\n';
	
	result = 1;
	}

catch (...) {
	llvm::WithColor::error(llvm::errs(), argv[0]) << "error" << '\n';
	
	result = 1;
	}

return result;
}

