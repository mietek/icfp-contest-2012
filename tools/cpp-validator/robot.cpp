#include "robot.h"

void Robot::execute(const Robot::Command &cmd)
{
  switch (cmd) {
    case Left:
      map.moveRobot(map.robotPosition().left());
      break;
    case Right:
      map.moveRobot(map.robotPosition().right());
      break;
    case Up:
      map.moveRobot(map.robotPosition().up());
      break;
    case Down:
      map.moveRobot(map.robotPosition().down());
      break;
    case Abort:
      map.abort();
      break;
  }
}
