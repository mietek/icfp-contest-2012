// ---------------------------------------------------------------------------
// Public
// ---------------------------------------------------------------------------

#define O_ROBOT       'R'
#define O_WALL        '#'
#define O_ROCK        '*'
#define O_LAMBDA      '\\'
#define O_CLOSED_LIFT 'L'
#define O_OPEN_LIFT   'O'
#define O_EARTH       '.'
#define O_EMPTY       ' '

#define M_LEFT        'L'
#define M_RIGHT       'R'
#define M_UP          'U'
#define M_DOWN        'D'
#define M_WAIT        'W'
#define M_ABORT       'A'

#define C_NONE        'N'
#define C_WIN         'W'
#define C_LOSE        'L'
#define C_ABORT       'A'


struct state *new(long input_length, const char *input);
struct state *new_from_file(const char *path);
struct state *copy(const struct state *s0);
bool equal(const struct state *s1, const struct state *s2);

void dump(const struct state *s);

void get_world_size(const struct state *s, long *out_world_w, long *out_world_h);
void get_robot_point(const struct state *s, long *out_robot_x, long *out_robot_y);
void get_lift_point(const struct state *s, long *out_lift_x, long *out_lift_y);
long get_water_level(const struct state *s);
long get_flooding_rate(const struct state *s);
long get_robot_waterproofing(const struct state *s);
long get_used_robot_waterproofing(const struct state *s);
long get_lambda_count(const struct state *s);
long get_collected_lambda_count(const struct state *s);
long get_move_count(const struct state *s);
long get_score(const struct state *s);
char get_condition(const struct state *s);
char get(const struct state *s, long x, long y);

struct state *make_one_move(const struct state *s0, char move);
struct state *make_moves(const struct state *s0, const char *moves);


// ---------------------------------------------------------------------------
// Private
// ---------------------------------------------------------------------------

#define LOG(fmt, ...) \
    do { \
        fprintf(stderr, fmt, ##__VA_ARGS__); \
        fflush(stderr); \
    } while (0)
#define LOG_EXIT(fmt, ...) \
    do { \
        LOG(fmt, ##__VA_ARGS__); \
        exit(1); \
    } while (0)
#define PERROR_EXIT(msg) \
    do { \
        perror(msg); \
        exit(1); \
    } while (0)
#define ASSERT(val) assert(val)

#if DEBUG
#define DEBUG_LOG(fmt, ...) LOG(fmt, ##__VA_ARGS__)
#define DEBUG_ASSERT(val) ASSERT(val)
#else
#define DEBUG_LOG(fmt, ...)
#define DEBUG_ASSERT(val)
#endif


#define DEFAULT_ROBOT_WATERPROOFING 10

struct state {
    long world_w, world_h;
    long robot_x, robot_y;
    long lift_x, lift_y;
    long water_level;
    long flooding_rate;
    long robot_waterproofing;
    long used_robot_waterproofing;
    long lambda_count;
    long collected_lambda_count;
    long move_count;
    long score;
    char condition;
    long world_length;
    char world[];
};


inline void make_point(struct state *s, long w, long h, long *out_x, long *out_y) {
    DEBUG_ASSERT(s && out_x && out_y);
    *out_x = w + 1;
    *out_y = s->world_h - h;
}

inline long unmake_point(const struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    long w, h, i;
    w = x - 1;
    DEBUG_ASSERT(w >= 0 && w < s->world_w);
    h = s->world_h - y;
    DEBUG_ASSERT(h >= 0 && h < s->world_h);
    i = h * (s->world_w + 1) + w;
    DEBUG_ASSERT(i < s->world_length);
    return i;
}

inline bool is_valid_move(char move) {
    return move == M_LEFT || move == M_RIGHT || move == M_UP || move == M_DOWN || move == M_WAIT || move == M_ABORT;
}


void scan_input(long input_length, const char *input, long *out_world_w, long *out_world_h, long *out_world_length);

void copy_input_metadata(struct state *s, long input_length, const char *input);
void copy_input(struct state *s, long input_length, const char *input);

void put(struct state *s, long x, long y, char object);

void move_robot(struct state *s, long x, long y);
void collect_lambda(struct state *s);
void execute_move(struct state *s, char move);
void update_world(struct state *s, const struct state *t);
