#include "map.h"

using std::string;

std::istream &Map::operator<<(std::istream &stream) 
{
  string tmp;
  
  int y = 0;
  
  while (!stream.eof()) {
    getline(stream, tmp);
    data.push_back(tmp);
    int x = analyze(tmp);
    if (x >= 0)
      _robotPosition = Position(x, y);
    
    y++;
  }
  
  return stream;
}

std::ostream &operator<<(std::ostream &stream, const Map &map)
{
  for (auto it = map.data.begin(); it != map.data.end(); ++it)
    stream << *it << std::endl;
  return stream;
}

void Map::update() 
{
  MapData newMap(data);

  for (unsigned int y = 0; y < data.size(); ++y) {
    for (unsigned int x = 0; x < data[y].size(); ++x) {
      Position pos{x, y};
      if (tile(pos) == Rock) {
        Position rockPos{pos};
        if (tile(pos.down()) == Empty) {
          rockPos = pos.down();
        } else if (tile(pos.down()) == Rock) {
          if (tile(pos.right()) == Empty && tile(pos.down().right()) == Empty) {
            rockPos = pos.down().right();
          } else if (tile(pos.left()) == Empty && tile(pos.down().left()) == Empty) {
            rockPos = pos.down().left();
          }
        } else if (tile(pos.down()) == Lambda && tile(pos.right()) == Empty && tile(pos.right().down()) == Empty) {
          rockPos = pos.down().right();
        }
        
        if (rockPos != pos) {
          newMap.tile(pos) = Empty;
          newMap.tile(rockPos) = Rock;
          if (tile(rockPos.down()) == Robot)
            _robotHit = true;
        }
      } else if (tile(pos) == ClosedLift && !_lambdas)
        newMap.tile(pos) = OpenLift;
    }
  }
  
  data = newMap;
}

bool Map::done() const
{
  return _won || _robotHit || _aborted;
}

std::string Map::condition() const
{
  if (_aborted)
    return "aborted";
  if (_robotHit)
    return "robot hit";
  if (_won)
    return "won";
  return std::string();
}

bool Map::moveRobot(const Position &newPos)
{
  _moves++;
  
  if (newPos.x < 0 || newPos.y < 0)
    return false;
  
  Tile target = data[newPos.y][newPos.x];
  
  bool moved = false;
  
  switch (target) {
    case Robot:
    case ClosedLift:
    case Wall:
      break;
      
    case Lambda:
      _lambdas--;
      _collected++;
      moved = true;
      break;
      
    case OpenLift:
      _won = true;
      moved = true;
      break;
      
    case Earth:
    case Empty:
      moved = true;
      break;
      
    case Rock:
      if (newPos == robotPosition().right() && tile(newPos.right()) == Empty) {
        moved = true;
        tile(newPos.right()) = Rock;
      } else if (newPos == robotPosition().left() && tile(newPos.left()) == Empty) {
        moved = true;
        tile(newPos.left()) = Rock;
      }
      break;
  }
  
  if (moved) {
    tile(robotPosition()) = Empty;
    tile(newPos) = Robot;
    _robotPosition = newPos;
  }
  
  return moved;
}

int Map::score() const
{
  int score = _collected * 25 - _moves;
  if (_won)
    return score + _collected * 50;
  if (_robotHit)
    return score;
  if (_aborted)
    return score + _collected * 25;
  
  return score;
}

int Map::analyze(const std::string &line)
{
  int robot = -1;
  
  for (unsigned int i = 0; i < line.size(); ++i)
    if (line[i] == Lambda)
      _lambdas++;
    else if (line[i] == Robot)
      robot = i;
    
  return robot;
}
