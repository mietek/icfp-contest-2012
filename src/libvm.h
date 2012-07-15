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
#define O_BEARD       'W'
#define O_RAZOR       '!'

#define O_FIRST_TRAMPOLINE 'A'
#define O_LAST_TRAMPOLINE  'I'

#define O_FIRST_TRAMPOLINE_TARGET '1'
#define O_LAST_TRAMPOLINE_TARGET  '9'

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
long get_beard_growth_rate(const struct state *s);
long get_razors_count(const struct state *s);
long get_lambda_count(const struct state *s);
long get_collected_lambda_count(const struct state *s);
long get_trampoline_count(const struct state *s);
bool get_trampoline_point(const struct state *s, char trampoline, long *out_trampoline_x, long *out_trampoline_y);
bool get_target_point(const struct state *s, char target, long *out_target_x, long *out_target_y);
bool get_trampoline_target(const struct state *s, char trampoline, char *out_target);
long get_move_count(const struct state *s);
long get_score(const struct state *s);
char get_condition(const struct state *s);
char safe_get(const struct state *s, long x, long y);

struct state *make_one_move(const struct state *s0, char move);
struct state *make_moves(const struct state *s0, const char *moves);

struct state *update_world_ignoring_robot(const struct state *s0);
struct state *imagine_robot_at(const struct state *s0, long x, long y);

bool is_enterable(const struct state *s, long x, long y);
bool is_safe(const struct state *s0, long x, long y);


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
#define DEFAULT_BEARD_GROWTH_RATE 25
#define MAX_TRAMPOLINE_COUNT 9


#define IGNORE_ROBOT        true
#define DO_NOT_IGNORE_ROBOT false


struct state {
    long world_w, world_h;
    long robot_x, robot_y;
    long lift_x, lift_y;
    long water_level;
    long flooding_rate;
    long robot_waterproofing;
    long used_robot_waterproofing;
    long beard_growth_rate;
    long razors_count;
    long lambda_count;
    long collected_lambda_count;
    long trampoline_count;
    long trampoline_x[MAX_TRAMPOLINE_COUNT + 1], trampoline_y[MAX_TRAMPOLINE_COUNT + 1];
    long target_x[MAX_TRAMPOLINE_COUNT + 1], target_y[MAX_TRAMPOLINE_COUNT + 1];
    long trampoline_index_to_target_index[MAX_TRAMPOLINE_COUNT+ 1];
    long move_count;
    long score;
    char condition;
    long world_length;
    char world[];
};


inline bool is_valid_point(long x, long y) {
    return x >= 1 && y >= 1;
}

inline bool is_valid_move(char move) {
    return move == M_LEFT || move == M_RIGHT || move == M_UP || move == M_DOWN || move == M_WAIT || move == M_ABORT;
}

inline bool is_valid_trampoline(char trampoline) {
    return trampoline >= O_FIRST_TRAMPOLINE && trampoline <= O_LAST_TRAMPOLINE;
}

inline bool is_valid_target(char target) {
    return target >= O_FIRST_TRAMPOLINE_TARGET && target <= O_LAST_TRAMPOLINE_TARGET;
}


inline bool is_within_world(const struct state *s, long x, long y) {
    return is_valid_point(x, y) && x <= s->world_w && y <= s->world_h;
}


inline void size_to_point(const struct state *s, long w, long h, long *out_x, long *out_y) {
    DEBUG_ASSERT(s && out_x && out_y);
    *out_x = w + 1;
    *out_y = s->world_h - h;
}

inline void point_to_size(const struct state *s, long x, long y, long *out_w, long *out_h) {
    DEBUG_ASSERT(s && out_w && out_h);
    *out_w = x - 1;
    *out_h = s->world_h - y;
}


inline long point_to_index(const struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    long w, h, i;
    point_to_size(s, x, y, &w, &h);
    i = h * (s->world_w + 1) + w;
    return i;
}


inline char index_to_trampoline(long i) {
    return O_FIRST_TRAMPOLINE + i - 1;
}

inline long trampoline_to_index(char trampoline) {
    DEBUG_ASSERT(is_valid_trampoline(trampoline));
    return trampoline - O_FIRST_TRAMPOLINE + 1;
}


inline char index_to_target(long i) {
    return O_FIRST_TRAMPOLINE_TARGET + i - 1;
}

inline long target_to_index(char target) {
    DEBUG_ASSERT(is_valid_target(target));
    return target - O_FIRST_TRAMPOLINE_TARGET + 1;
}


inline char get(const struct state *s, long x, long y) {
    DEBUG_ASSERT(s && is_within_world(s, x, y));
    return s->world[point_to_index(s, x, y)];
}

inline void put(struct state *s, long x, long y, char object) {
    DEBUG_ASSERT(s && is_within_world(s, x, y));
    s->world[point_to_index(s, x, y)] = object;
}


void scan_input(long input_length, const char *input, long *out_world_w, long *out_world_h, long *out_world_length);

void copy_input_metadata(struct state *s, long input_length, const char *input);
void copy_input(struct state *s, long input_length, const char *input);

void teleport_robot(struct state *s, long x, long y);
void move_robot(struct state *s, long x, long y);
void collect_lambda(struct state *s);
void clear_similar_trampolines(struct state *s, char trampoline);
void execute_move(struct state *s, char move);
void update_world(struct state *s, const struct state *t, bool ignore_robot);
