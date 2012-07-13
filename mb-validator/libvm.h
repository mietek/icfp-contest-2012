struct state;


struct state *new(long input_size, const char *input);
struct state *new_from_file(const char *path);

void dump(const struct state *s);

void get_world_size(const struct state *s, long *out_world_w, long *out_world_h);
void get_robot_point(const struct state *s, long *out_robot_x, long *out_robot_y);
void get_lift_point(const struct state *s, long *out_lift_x, long *out_lift_y);
long get_lambda_count(const struct state *s);
long get_move_count(const struct state *s);
char get_condition(const struct state *s);
char lookup_point(const struct state *s, long x, long y);

struct state *make_one_move(const struct state *s0, char move);
struct state *make_moves(const struct state *s0, const char *moves);
