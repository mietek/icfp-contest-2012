#include "solution.h"

#include <algorithm>

static const char *MOVES = "LRUDWS";
#define NMOVES 6

static int rand_int(const int max)
{
  return std::rand() * (1.0 / (RAND_MAX + 1.0 )) * max;
}

struct rnd_gen {
    char operator ()() const {
        return MOVES[static_cast<std::size_t>(rand_int(NMOVES))];
    }
};

#define MAX_INITIAL_LENGTH 10000

Solution::Solution()
{
  const int len = rand_int(MAX_INITIAL_LENGTH);
  _moves.reserve(len);
  std::generate_n(std::back_inserter(_moves), len, rnd_gen());
}
