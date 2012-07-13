#include <fstream>
#include <iostream>

#include "map.h"
#include "robot.h"

int main(int argc, char **argv)
{
  bool vvMode = false;
  
  if (argc < 2) {
    std::cerr << "No arguments given." << std::endl;
    return 1;
  }
  
  int fileArg = 1;
  
  if (argc == 3 && std::string(argv[1]) == std::string("-vv")) {
    vvMode = true;
    fileArg = 2;
  }
  
  std::fstream mapFile{argv[fileArg], std::fstream::in};
  if (mapFile.fail()) {
    std::cerr << "Could not open map." << std::endl;
    return 2;
  }
  
  Map m;
  m << mapFile;
  
  Robot r{m};
  Robot::Command c;
  
  do {
    std::cin.get(c);
    if (std::cin.eof())
      c = Robot::Abort;
    r.execute(c);
    m.update();
    if (!vvMode)
      std::cout << m;
  } while (!m.done());
  
  if (vvMode) {
    std::cout << m.score() << std::endl;
    std::cout << m;
  } else {
    std::cout << "Score: " << m.score() << "; " << m.condition() << std::endl;
  }
  
  return 0;
}
