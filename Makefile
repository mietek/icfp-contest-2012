all: lambdamine

CXXFLAGS+=-std=gnu++0x

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^

lambdamine: main.o map.o robot.o
	$(CXX) -o $@ $^

.PHONY: all
