#include <iostream>

#include "vm.h"

int main(int argc, char ** argv)
{
  std::string map((std::istreambuf_iterator<char>(std::cin)), 
    std::istreambuf_iterator<char>());
  
  VM vm(map);
  std::cout << vm.score("DL") << std::endl;
  return 0;
}
