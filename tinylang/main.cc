#include <llvm/ADT/Triple.h>
#include <llvm/CodeGen/CommandFlags.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/ToolOutputFile.h>
#include <llvm/Support/WithColor.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetOptions.h>

#include "Diagnostic.h"
#include "Generate.h"
#include "Parser.h"


using namespace std::literals::string_literals;


/*	gProgramName
	String by which we were invoked
*/
static const char *gProgramName;


static const char
	gHead[] = "tinylang -- Tinylang compiler",
	gVersion[] = "0.9";


static llvm::cl::list<std::string> gInputFiles(
	llvm::cl::Positional,
	llvm::cl::desc("<input-files>")
	);

static llvm::cl::opt<std::string> gModuleTriple(
	"mtriple",
	llvm::cl::desc("Override target triple for module")
	);

static llvm::cl::opt<bool> gEmitLLVM(
	"emit-llvm",
	llvm::cl::desc("emit IR instead of assembler"),
	llvm::cl::init(false)
	);


// register codegen-related command-line options
static llvm::codegen::RegisterCodeGenFlags gCodeGenerationFlags;



/*	PrintVersion

*/
static void PrintVersion(
	llvm::raw_ostream &output
	)
{
output <<
	gHead << ' ' << gVersion << '\n' <<
	"default target: " << llvm::sys::getDefaultTargetTriple() << '\n' <<
	"host CPU: " << llvm::sys::getHostCPUName() << '\n';
output.flush();

llvm::TargetRegistry::printRegisteredTargetsForVersion(output);
}


/*	createTargetMachine

*/
static llvm::TargetMachine *createTargetMachine(
	const char	arg[]
	)
{
// autoconf configuration name ('triple') that we're targeting
llvm::Triple triple(
	// user provided one in command-line argument?
	!gModuleTriple.empty() ?
		// 'normalize' that
		llvm::Triple::normalize(gModuleTriple) :
		
		// default target triple we have been configured to generate code for
		llvm::sys::getDefaultTargetTriple()
	);

// get default code generation options for the target platform
llvm::TargetOptions targetOptions = llvm::codegen::InitTargetOptionsFromCodeGenFlags(triple);

// get target-specific information
std::string error;
const llvm::Target *target = llvm::TargetRegistry::lookupTarget(llvm::codegen::getMArch(), triple, error);
if (!target) throw std::runtime_error(error);

// create target-specific machine implementation
llvm::TargetMachine *targetMachine = target->createTargetMachine(
	triple.getTriple(),
	llvm::codegen::getCPUStr(),
	llvm::codegen::getFeaturesStr(),
	targetOptions,
	llvm::Optional<llvm::Reloc::Model>(llvm::codegen::getRelocModel())
	);
if (!targetMachine) throw "can't create target machine";

return targetMachine;
}


/*	calculateOutputFileName

*/
static std::string calculateOutputFileName(
	llvm::StringRef inputFileName
	)
{
std::string outputFileName;

if (inputFileName == "-")
	outputFileName= "-";

else {
	if (inputFileName.endswith(".mod"))
		outputFileName = inputFileName.drop_back(4).str();
	
	else
		outputFileName = inputFileName.str();
	
	std::string suffix;
	switch (llvm::codegen::getFileType()) {
		case llvm::CGFT_AssemblyFile:
			suffix = gEmitLLVM ? ".ll" : ".s"; break;
		
		case llvm::CGFT_ObjectFile:
			suffix = ".o"; break;
		
		case llvm::CGFT_Null:
			suffix = ".null"; break;
		}
	outputFileName.append(suffix);
	}

return outputFileName;
}


/*	emit

*/
static void emit(
	llvm::Module	*module,
	llvm::TargetMachine &machine,
	llvm::StringRef	inputFileName
	)
{
const bool isFileTypeAssembly = llvm::codegen::getFileType() == llvm::CGFT_AssemblyFile;

// open the file
std::error_code error;
std::unique_ptr<llvm::ToolOutputFile> outputFile = std::make_unique<llvm::ToolOutputFile>(
	calculateOutputFileName(inputFileName),
	error,
	isFileTypeAssembly ? llvm::sys::fs::OF_Text : llvm::sys::fs::OF_None
	);
if (error) throw error;

llvm::legacy::PassManager passManager;
if (isFileTypeAssembly)
	passManager.add(llvm::createPrintModulePass(outputFile->os()));

else
	if (machine.addPassesToEmitFile(passManager, outputFile->os(), nullptr, llvm::codegen::getFileType()))
		throw "no support for file type";

passManager.run(*module);
outputFile->keep();
}


/*	CompileOne

*/
static void CompileOne(
	llvm::TargetMachine &targetMachine,
	const std::string file
	)
{
// get the contents of the file into a memory buffer
llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileOrErr = llvm::MemoryBuffer::getFile(file);
if (std::error_code BufferError = fileOrErr.getError())
	throw std::runtime_error("reading: "s + file + ": " + BufferError.message());
std::unique_ptr<llvm::MemoryBuffer> &fileBuffer = *fileOrErr;

// collect source files
llvm::SourceMgr sources;

// collect diagnostics
Diagnostics diagnostics(sources);

// add source file memory buffer to source manager; which takes ownership of the buffer
sources.AddNewSourceBuffer(std::move(fileBuffer), llvm::SMLoc());

// obtain source file back again as string reference
llvm::StringRef buffer = sources.getMemoryBuffer(
	sources.getMainFileID()
	)->getBuffer();

// parse the source file
ModuleDeclaration *module = Parser(diagnostics, buffer)();
if (!module) throw "can't parse into module declaration";
if (diagnostics.numErrors() > 0) throw "parsing produced errors";

// per-thread LLVM data
llvm::LLVMContext context;

// generate LLVM structures
std::unique_ptr<llvm::Module> m = Generator(context)(targetMachine, file, *module);

// emit target machine code
emit(m.get(), targetMachine, file);
}


/*	main
	Command-line entry point
*/
int main(
	int		argc,
	char		*argv[]
	)
{
gProgramName = argv[0];

// 'C' locale; no internationalization
if (!setlocale(LC_ALL, "C")) {
	llvm::errs() << "can't set locale\n";
	return 2;
	}

int result = 0;

llvm::InitLLVM initialize(argc, argv);
llvm::SmallVector<const char *, 256> args(argv + 1, argv + argc);

LLVMInitializeX86TargetInfo();
LLVMInitializeX86Target();
LLVMInitializeX86TargetMC();
LLVMInitializeX86AsmPrinter();
LLVMInitializeX86AsmParser();

// function that reacts to '--version'
llvm::cl::SetVersionPrinter(&PrintVersion);

// parse command-line options
llvm::cl::ParseCommandLineOptions(argc, argv, gHead);

// *** MCPU or MAttrs == help

llvm::TargetMachine *const targetMachine = createTargetMachine(argv[0]);

// for each file named by command-line argument
for (const std::string &file: gInputFiles)
	try {
		CompileOne(*targetMachine, file);
		}
	
	catch (const std::runtime_error &error) {
		llvm::WithColor::error(llvm::errs(), gProgramName) << error.what() << '\n';
		
		result = 1;
		}
	
	catch (const std::error_code &error) {
		llvm::WithColor::error(llvm::errs(), gProgramName) << error.message() << '\n';
		
		result = 1;
		}
	
	catch (const char error[]) {
		llvm::WithColor::error(llvm::errs(), gProgramName) << error << '\n';
		
		result = 1;
		}
	
	catch (...) {
		llvm::WithColor::error(llvm::errs(), gProgramName) << "error" << '\n';
		
		result = 1;
		}

return result;
}
