#include <assert.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "libvm.h"


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

#if 1
#define DEBUG_LOG(fmt, ...) LOG(fmt, ##__VA_ARGS__)
#define DEBUG_ASSERT(val) ASSERT(val)
#else
#define DEBUG_LOG(fmt, ...)
#define DEBUG_ASSERT(val)
#endif


#define O_ROBOT       'R'
#define O_WALL        '#'
#define O_ROCK        '*'
#define O_LAMBDA      '\\'
#define O_CLOSED_LIFT 'L'
#define O_OPEN_LIFT   'O'
#define O_EARTH       '.'
#define O_EMPTY       ' '

#define M_LEFT  'L'
#define M_RIGHT 'R'
#define M_UP    'U'
#define M_DOWN  'D'
#define M_WAIT  'W'
#define M_ABORT 'A'

enum {
    C_NONE,
    C_WIN,
    C_LOSE,
    C_ABORT
};

struct state {
    long world_w, world_h;
    long robot_x, robot_y;
    long lift_x, lift_y;
    long lambda_count;
    long collected_lambda_count;
    long move_count;
    long score;
    char condition;
    long world_length;
    char world[];
};


static void scan_input(long input_length, const char *input, long *out_world_w, long *out_world_h, long *out_world_length) {
    DEBUG_ASSERT(input && out_world_w && out_world_h);
    long world_w = 0, world_h = 0, w = 0, i;
    for (i = 0; i < input_length; i++) {
        if (input[i] == '\n') {
            if (w > world_w)
                world_w = w;
            world_h++;
            w = 0;
        } else
            w++;
    }
    *out_world_w = world_w;
    *out_world_h = world_h;
    *out_world_length = (world_w + 1) * world_h + 1;
}


inline static void make_point(struct state *s, long w, long h, long *out_x, long *out_y) {
    DEBUG_ASSERT(s && out_x && out_y);
    *out_x = w + 1;
    *out_y = s->world_h - h;
}


static void copy_input(struct state *s, long input_length, const char* input) {
    DEBUG_ASSERT(s && input);
    long w = 0, h = 0, j = 0, i;
    for (i = 0; i < input_length; i++) {
        if (input[i] == '\n') {
            while (w < s->world_w) {
                s->world[j++] = O_EMPTY;
                w++;
            }
            s->world[j++] = '\n';
            h++;
            w = 0;
        } else {
            if (input[i] == O_ROBOT)
                make_point(s, w, h, &s->robot_x, &s->robot_y);
            else if (input[i] == O_LAMBDA)
                s->lambda_count++;
            else if (input[i] == O_CLOSED_LIFT)
                make_point(s, w, h, &s->lift_x, &s->lift_y);
            s->world[j++] = input[i];
            w++;
        }
    }
    s->world[j] = 0;
}


struct state *new(long input_length, const char *input) {
    DEBUG_ASSERT(input);
    long world_w, world_h, world_length;
    struct state *s;
    scan_input(input_length, input, &world_w, &world_h, &world_length);
    if (!(s = malloc(sizeof(struct state) + world_length)))
        PERROR_EXIT("malloc");
    memset(s, 0, sizeof(struct state));
    s->world_w = world_w;
    s->world_h = world_h;
    s->world_length = world_length;
    copy_input(s, input_length, input);
    return s;
}


struct state *new_from_file(const char *path) {
    DEBUG_ASSERT(path);
    int fd;
    struct stat info;
    char *input;
    struct state *s;
    if ((fd = open(path, O_RDONLY)) == -1)
        PERROR_EXIT("open");
    if (fstat(fd, &info) == -1)
        PERROR_EXIT("fstat");
    if ((input = mmap(0, info.st_size, PROT_READ, MAP_SHARED, fd, 0)) == (char *)-1)
        PERROR_EXIT("mmap");
    s = new(info.st_size, input);
    munmap(input, info.st_size);
    close(fd);
    return s;
}


void dump(const struct state *s) {
    DEBUG_ASSERT(s);
    fputs("----------------------------------------------------------------\n", stderr);
    fprintf(stderr, "world_size             = (%ld, %ld)\n", s->world_w, s->world_h);
    fprintf(stderr, "robot_point            = (%ld, %ld)\n", s->robot_x, s->robot_y);
    fprintf(stderr, "lift_point             = (%ld, %ld)\n", s->lift_x, s->lift_y);
    fprintf(stderr, "lambda_count           = %ld\n", s->lambda_count);
    fprintf(stderr, "collected_lambda_count = %ld\n", s->collected_lambda_count);
    fprintf(stderr, "move_count             = %ld\n", s->move_count);
    fprintf(stderr, "score                  = %ld\n", s->score);
    fprintf(stderr, "condition              = %d\n", s->condition);
    fprintf(stderr, "world_length           = %ld\n\n", s->world_length);
    fputs(s->world, stderr);
    fputc('\n', stderr);
}


struct state *copy(const struct state *s0) {
    DEBUG_ASSERT(s0);
    struct state *s;
    if (!(s = malloc(sizeof(struct state) + s0->world_length)))
        PERROR_EXIT("malloc");
    memcpy(s, s0, sizeof(struct state) + s0->world_length);
    return s;
}


void get_world_size(const struct state *s, long *out_world_w, long *out_world_h) {
    DEBUG_ASSERT(s && out_world_w && out_world_h);
    *out_world_w = s->world_w;
    *out_world_h = s->world_h;
}


void get_robot_point(const struct state *s, long *out_robot_x, long *out_robot_y) {
    DEBUG_ASSERT(s && out_robot_x && out_robot_y);
    *out_robot_x = s->robot_x;
    *out_robot_y = s->robot_y;
}


void get_lift_point(const struct state *s, long *out_lift_x, long *out_lift_y) {
    DEBUG_ASSERT(s && out_lift_x && out_lift_y);
    *out_lift_x = s->lift_x;
    *out_lift_y = s->lift_y;
}


long get_lambda_count(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->lambda_count;
}


long get_collected_lambda_count(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->collected_lambda_count;
}


long get_move_count(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->move_count;
}


long get_score(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->score;
}


char get_condition(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->condition;
}


inline static long unmake_point(const struct state *s, long x, long y) {
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


inline char get(const struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    long i;
    i = unmake_point(s, x, y);
    return s->world[i];
}


inline static void put(struct state *s, long x, long y, char object) {
    DEBUG_ASSERT(s);
    long i;
    i = unmake_point(s, x, y);
    s->world[i] = object;
}


inline static void move_robot(struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    DEBUG_ASSERT(get(s, x, y) == O_EMPTY || get(s, x, y) == O_EARTH || get(s, x, y) == O_LAMBDA || get(s, x, y) == O_OPEN_LIFT || (get(s, x, y) == O_ROCK && get(s, x + x - s->robot_x, y) == O_EMPTY));
    put(s, s->robot_x, s->robot_y, O_EMPTY);
    s->robot_x = x;
    s->robot_y = y;
    put(s, x, y, O_ROBOT);
    DEBUG_LOG("moved to (%ld, %ld)\n", x, y);
}


inline static void collect_lambda(struct state *s) {
    DEBUG_ASSERT(s);
    DEBUG_ASSERT(s->collected_lambda_count < s->lambda_count);
    DEBUG_ASSERT(get(s, s->lift_x, s->lift_y) == O_CLOSED_LIFT);
    s->collected_lambda_count++;
    s->score += 25;
    DEBUG_LOG("lambda collected\n");
}


static void execute_move(struct state *s, char move) {
    DEBUG_ASSERT(s);
    DEBUG_ASSERT(s->condition == C_NONE);
    if (move == M_LEFT || move == M_RIGHT || move == M_UP || move == M_DOWN) {
        long x, y;
        char object;
        if (move == M_LEFT) {
            x = s->robot_x - 1;
            y = s->robot_y;
        } else if (move == M_RIGHT) {
            x = s->robot_x + 1;
            y = s->robot_y;
        } else if (move == M_UP) {
            x = s->robot_x;
            y = s->robot_y + 1;
        } else if (move == M_DOWN) {
            x = s->robot_x;
            y = s->robot_y - 1;
        }
        object = get(s, x, y);
        if (object == O_EMPTY || object == O_EARTH)
            move_robot(s, x, y);
        else if (object == O_LAMBDA) {
            move_robot(s, x, y);
            collect_lambda(s);
        } else if (object == O_OPEN_LIFT) {
            move_robot(s, x, y);
            s->score += s->collected_lambda_count * 50;
            s->condition = C_WIN;
            DEBUG_LOG("won\n");
        } else if (object == O_ROCK && move == M_LEFT && get(s, x - 1, y) == O_EMPTY) {
            move_robot(s, x, y);
            put(s, x - 1, y, O_ROCK);
            DEBUG_LOG("moved rock from (%ld, %ld) to (%ld, %ld)\n", x, y, x - 1, y);
        } else if (object == O_ROCK && move == M_RIGHT && get(s, x + 1, y) == O_EMPTY) {
            move_robot(s, x, y);
            put(s, x + 1, y, O_ROCK);
            DEBUG_LOG("moved rock from (%ld, %ld) to (%ld, %ld\n", x, y, x + 1, y);
        }
        else
            DEBUG_LOG("attempted invalid move '%c' from (%ld, %ld) to (%ld, %ld) which is '%c'\n", move, s->robot_x, s->robot_y, x, y, object);
        s->move_count++;
        s->score--;
    } else if (move == M_WAIT) {
        DEBUG_LOG("waited\n");
        s->move_count++;
        s->score--;
    } else if (move == M_ABORT) {
        s->score += s->collected_lambda_count * 25;
        s->condition = C_ABORT;
        DEBUG_LOG("aborted\n");
    }
}


static void update_world(struct state *s, const struct state *t) {
    DEBUG_ASSERT(s && t);
    long x, y;
    if (t->condition != C_NONE)
        return;
    for (y = 1; y <= s->world_h; y++) {
        for (x = 1; x <= s->world_w; x++) {
            char object;
            object = get(t, x, y);
            if (object == O_ROCK && get(t, x, y - 1) == O_EMPTY) {
                put(s, x, y, O_EMPTY);
                put(s, x, y - 1, O_ROCK);
                if (t->robot_x == x && t->robot_y == y - 2) {
                    s->condition = C_LOSE;
                    DEBUG_LOG("lost\n");
                }
            } else if (object == O_ROCK && get(t, x, y - 1) == O_ROCK && get(t, x + 1, y) == O_EMPTY && get(t, x + 1, y - 1) == O_EMPTY) {
                put(s, x, y, O_EMPTY);
                put(s, x + 1, y - 1, O_ROCK);
                if (t->robot_x == x + 1 && t->robot_y == y - 2) {
                    s->condition = C_LOSE;
                    DEBUG_LOG("lost\n");
                }
            } else if (object == O_ROCK && get(t, x, y - 1) == O_ROCK && (get(t, x + 1, y) != O_EMPTY || get(t, x + 1, y - 1) != O_EMPTY) && get(t, x - 1, y) == O_EMPTY && get(t, x - 1, y - 1) == O_EMPTY) {
                put(s, x, y, O_EMPTY);
                put(s, x - 1, y - 1, O_ROCK);
                if (t->robot_x == x - 1 && t->robot_y == y - 2) {
                    s->condition = C_LOSE;
                    DEBUG_LOG("lost\n");
                }
            } else if (object == O_ROCK && get(t, x, y - 1) == O_LAMBDA && get(t, x + 1, y) == O_EMPTY && get(t, x + 1, y - 1) == O_EMPTY) {
                put(s, x, y, O_EMPTY);
                put(s, x + 1, y - 1, O_ROCK);
                if (t->robot_x == x + 1 && t->robot_y == y - 2) {
                    s->condition = C_LOSE;
                    DEBUG_LOG("lost\n");
                }
            } else if (object == O_CLOSED_LIFT && t->collected_lambda_count == t->lambda_count) {
                put(s, x, y, O_OPEN_LIFT);
                DEBUG_LOG("lift opened\n");
            }
        }
    }
}


struct state *make_one_move(const struct state *s0, char move) {
    DEBUG_ASSERT(s0);
    struct state *s, *t;
    t = copy(s0);
    execute_move(t, move);
    s = copy(t);
    update_world(s, t);
    free(t);
    return s;
}


struct state *make_moves(const struct state *s0, const char *moves) {
    DEBUG_ASSERT(s0 && moves);
    struct state *s, *t;
    if (!*moves)
        s = copy(s0);
    else {
        s = (struct state *)s0;
        while (*moves) {
            t = copy(s);
            execute_move(t, *moves);
            s = copy(t);
            update_world(s, t);
            free(t);
            if (s->condition != C_NONE)
                break;
            moves++;
        }
    }
    return s;
}
