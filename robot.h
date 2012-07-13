#ifndef ROBOT_H
#define ROBOT_H

#include "map.h"

class Robot {
public:
  Robot(Map &_map) : map(_map), _aborted(false) {}
  
  typedef char Command;
  enum {
    Left = 'L',
    Right = 'R',
    Up = 'U',
    Down = 'D',
    Wait = 'W',
    Abort = 'A'
  } commands;

  void execute(const Command &cmd);
  
private:
  Map &map;
  bool _aborted;
};

#endif
