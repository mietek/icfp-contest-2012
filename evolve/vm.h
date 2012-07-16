#ifndef VM_H
#define VM_H

#include <string>

extern "C" {
#include "../src/libvm.h"
}

class VM {
public:
  VM(const std::string &map);
  int score(const std::string &moves) const;
  
private:
  state *_vm;
};

#endif
