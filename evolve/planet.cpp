#include "planet.h"

#include <algorithm>

#define POPULATION_SIZE 100

Planet::Planet(const VM &vm)
: _vm(vm)
{
  _population.reserve(POPULATION_SIZE);
  
  for (int i = 0; i < POPULATION_SIZE; ++i) {
    Solution sol;
    sol.score = _vm.score(sol.moves());
    _population.push_back(sol);
  }
  
  std::sort(_population.begin(), _population.end());
}

const std::string &Planet::best_moves() const
{
  return _population[POPULATION_SIZE - 1].moves();
}
