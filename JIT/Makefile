.PHONY: default
default: test-static test-dynamic test-jit test-lljit


CXXFLAGS := -I /usr/local/include/ -g
CPPFLAGS := --std=c++17 -fno-rtti -Wreorder


greetings.so: CXX:=clang
greetings.so: greetings.c
	$(CXX) -fPIC -shared -o $@ $<

greetings.o: CXX:=clang
greetings.o: greetings.c
	$(CXX) -c -o $@ $<

lljit: LIBS := \
		-lLLVMOrcJIT \
		-lLLVMIRReader \
		-lLLVMAArch64CodeGen \
		-lLLVMAArch64Desc \
		-lLLVMAArch64Info \
		-lLLVMAArch64AsmParser \
		-lLLVMCore \
		-lLLVMSupport \
		-lstdc++
lljit: lljit.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $^ $(LIBS)

jit: LIBS := \
		-lLLVMOrcJIT \
		-lLLVMExecutionEngine \
		-lLLVMAnalysis \
		-lLLVMPasses \
		-lLLVMIRReader \
		-lLLVMAArch64CodeGen \
		-lLLVMAArch64Desc \
		-lLLVMAArch64Info \
		-lLLVMAArch64AsmParser \
		-lLLVMCore \
		-lLLVMSupport \
		-lstdc++
jit: jit.cc jit.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $^ $(LIBS)


.PHONY: clean
clean:
	$(RM) -f greetings.o greetings.so lljit jit

.PHONY: test-static
test-static: greetings.o
	lli -extra-object greetings.o main.ll

.PHONY: test-dynamic
test-dynamic: greetings.so
	lli -load ./greetings.so main.ll

.PHONY: test-jit
test-jit: jit
	./jit hello.ll

.PHONY: test-lljit
test-lljit: lljit
	./lljit hello.ll

