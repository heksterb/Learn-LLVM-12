.phony: default
default: gcd Point.o


CXXFLAGS := -I /usr/local/include/ -g
CPPFLAGS := --std=c++17 -fno-rtti
LIBS := \
	-lLLVMX86AsmParser \
	-lLLVMX86CodeGen \
	-lLLVMX86Desc \
	-lLLVMX86Info \
	-lLLVMMCDisassembler \
	-lLLVMCFGuard \
	-lLLVMInstrumentation \
	-lLLVMDWARFLinker \
	-lLLVMGlobalISel \
	-lLLVMAsmPrinter \
	-lLLVMSelectionDAG \
	-lLLVMCodeGen \
	-lLLVMTarget \
	-lLLVMScalarOpts \
	-lLLVMTransformUtils \
	-lLLVMAnalysis \
	-lLLVMProfileData \
	-lLLVMDebugInfoDWARF \
	-lLLVMObject \
	-lLLVMTextAPI \
	-lLLVMMCParser \
	-lLLVMMC \
	-lLLVMDebugInfoCodeView \
	-lLLVMBitReader \
	-lLLVMCore \
	-lLLVMRemarks \
	-lLLVMBitstreamReader \
	-lLLVMBinaryFormat \
	-lLLVMSupport \
	-lLLVMDemangle \
	-lpthread -ldl


# pattern rule to invoke our compiler
%.o: %.mod tinylang
	./tinylang --filetype=obj $<


gcd: gcd.c Gcd.o
	$(CC) $^ -o $@

tinylang: main.cc Diagnostic.o Tokens.o Lexer.o Parser.o Semantic.o Generate.o AST.o
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $^ $(LIBS)

Diagnostic.o: Diagnostic.cc Diagnostic.h Diagnostic.def

Tokens.o: Tokens.cc Tokens.h Tokens.def

Lexer.o: Lexer.cc Lexer.h Tokens.h

Parser.o: Parser.cc Parser.h AST.h Lexer.h Semantic.h Tokens.h

Semantic.o: Semantic.cc Semantic.h AST.h

Generate.o: Generate.cc Generate.h AST.h

AST.o: AST.cc AST.h

.PHONY: clean
clean:
	rm -f gcd gcd.o tinylang Diagnostic.o Tokens.o Lexer.o Parser.o Semantic.o Generate.o AST.o
