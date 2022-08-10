.phony: default
default: countir.so


CXXFLAGS := -I /usr/local/include/ -g
CPPFLAGS := --std=c++17 -fno-rtti -Wreorder -fPIC
LIBS := \
	-lLLVMCore \
	-lLLVMRemarks \
	-lLLVMBitstreamReader \
	-lLLVMBinaryFormat \
	-lLLVMSupport \
	-lLLVMDemangle \
	-lpthread -ldl


countir.so: CXXFLAGS += -shared
countir.so: countir.o
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $^ $(LIBS)

countir.o: countir.cc

.PHONY: clean
clean:
	rm -f countir.so countir.o
