#include <fstream>
#include <iostream>

#include "map.h"
#include "robot.h"

int main(int argc, char **argv)
{
  if (argc != 2) {
    std::cerr << "No map given." << std::endl;
    return 1;
  }
  
  std::fstream mapFile{argv[1], std::fstream::in};
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
  } while (!m.done());
  
  std::cout << "Total score: " << m.score() << std::endl;
  
  return 0;
}
