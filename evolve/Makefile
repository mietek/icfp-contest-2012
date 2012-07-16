CXXFLAGS+=-std=c++0x -W -Wall

OBJS = main.o 
TARGET = evolve

all: compile_vm $(TARGET)

compile_vm:
	$(MAKE) -C .. bin/libvm.o

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^

$(TARGET): $(OBJS) ../bin/libvm.o vm.o
	$(CXX) -o $@ $^

.PHONY: all clean

clean:
	rm $(TARGET) $(OBJS)
