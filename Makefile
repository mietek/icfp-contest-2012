all: lambdamine

CXXFLAGS+=-std=gnu++0x

OBJS = main.o map.o robot.o

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^

lambdamine: $(OBJS)
	$(CXX) -o $@ $^

.PHONY: all clean

clean:
	rm lambdamine $(OBJS)
