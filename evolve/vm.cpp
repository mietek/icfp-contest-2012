#include "vm.h"

extern "C" {
#include "../src/libvm.h"
}

VM::VM(const std::string &map)
{
  _vm = new_vm(map.length(), map.data());
}

int VM::score(const std::string &moves) const
{
  state *result = make_moves(_vm, moves.c_str());
  int score = get_score(result);
  delete_vm(result);
  return score;
}
