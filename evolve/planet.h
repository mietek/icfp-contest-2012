#ifndef PLANET_H
#define PLANET_H

#include <vector>

#include "solution.h"
#include "vm.h"

class Solution;

class Planet {
public:
  Planet(const VM &vm);
  const std::string &best_moves() const;
  
private:
  const VM &_vm;
  
  typedef std::vector<Solution> Population;
  Population _population;
};

#endif
