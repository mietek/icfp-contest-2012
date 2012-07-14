#ifndef MAP_H
#define MAP_H

#include <istream>
#include <string>
#include <vector>

class Map {
public:
  Map() : 
    _lambdas(0), _collected(0), _moves(-1), 
    _water(0), _flooding(0), _waterproof(10),
    _drank(0), _ticks(0),
    _robotHit(false), _aborted(false), _won(false), _drowned(false),
    _vvMode(false) {}
  
  struct Position {
    int x, y;
    Position() : x(-1), y(-1) {}
    Position(int _x, int _y) : x(_x), y(_y) {}
    Position left() const {
      return Position(x - 1, y);
    }
    Position right() const {
      return Position(x + 1, y);
    }
    Position up() const {
      return Position(x, y - 1);
    }
    Position down() const {
      return Position(x, y + 1);
    }
    bool operator==(const Position &other) const {
      return x == other.x && y == other.y;
    }
    bool operator!=(const Position &other) const {
      return !((*this) == other);
    }
  };
  
  typedef char Tile;
  Tile &tile(const Position &pos) {
    return data.tile(pos);
  }
  
  enum {
    Robot = 'R',
    Wall = '#',
    Rock = '*',
    Lambda = '\\',
    ClosedLift = 'L',
    OpenLift = 'O',
    Earth = '.',
    Empty = ' '
  } tiles;

  std::istream &operator<<(std::istream &stream);
  
  void update();
  void abort() { _aborted = true; }
  bool done() const;
  int score() const;
  std::string condition() const;

  bool moveRobot(const Position &newPos);
  const Position &robotPosition() const { return _robotPosition; }
  void setVv(bool state = true) { _vvMode = state; }

private:
  class MapData : public std::vector<std::string> {
  public:
    Tile &tile(const Position &pos) {
      return (*this)[pos.y][pos.x];
    }
  };
  MapData data;
  
  int analyze(const std::string &line);
  void flood();
  bool robotUnderwater() const;
  void robotTakesWater();
  
  friend std::ostream &operator<<(std::ostream &stream, const Map &map);
  
  Position _robotPosition;
  int _lambdas, _collected, _moves;
  unsigned int _water, _flooding, _waterproof, _drank, _ticks;
  bool _robotHit, _aborted, _won, _drowned;
  bool _vvMode;
};

std::ostream &operator<<(std::ostream &stream, const Map &map);


#endif
