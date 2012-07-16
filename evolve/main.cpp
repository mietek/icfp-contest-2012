#include <iostream>

#include <cstdlib>
#include <ctime>

#include "planet.h"
#include "vm.h"

int main(int argc, char ** argv)
{
  std::srand(std::time(0));
  
  std::string map(
    std::istreambuf_iterator<char>(std::cin), 
    std::istreambuf_iterator<char>()
  );
  
  VM vm {map};
  
  Planet planet {vm};

  std::cout << planet.best_moves() << std::endl;
}
