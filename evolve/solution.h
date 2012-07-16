#ifndef SOLUTION_H
#define SOLUTION_H

#include <string>

#include "vm.h"

class Solution {
public:
  Solution();
  int score;
  const std::string &moves() const {
    return _moves;
  }
  
  bool operator<(const Solution &other) const {
    return score > other.score;
  }

private:
  std::string _moves;
};

#endif
