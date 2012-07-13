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
    long move_count;
    char condition;
    long world_size;
    char world[];
};


static void scan_input(long input_size, const char *input, long *out_world_w, long *out_world_h, long *out_world_size) {
    DEBUG_ASSERT(input && out_world_w && out_world_h);
    long world_w = 0, world_h = 0, w = 0, i;
    for (i = 0; i < input_size; i++) {
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
    *out_world_size = (world_w + 1) * world_h + 1;
}


inline static void make_point(struct state *s, long w, long h, long *out_x, long *out_y) {
    DEBUG_ASSERT(s && out_x && out_y);
    *out_x = w + 1;
    *out_y = s->world_h - h;
}


static void copy_input(struct state *s, long input_size, const char* input) {
    DEBUG_ASSERT(s && input);
    long w = 0, h = 0, j = 0, i;
    for (i = 0; i < input_size; i++) {
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
            else if (input[i] == O_CLOSED_LIFT)
                make_point(s, w, h, &s->lift_x, &s->lift_y);
            s->world[j++] = input[i];
            w++;
        }
    }
    s->world[j] = 0;
}


struct state *new(long input_size, const char *input) {
    DEBUG_ASSERT(input);
    long world_w, world_h, world_size;
    struct state *s;
    scan_input(input_size, input, &world_w, &world_h, &world_size);
    if (!(s = malloc(sizeof(struct state) + world_size)))
        PERROR_EXIT("malloc");
    memset(s, 0, sizeof(struct state));
    s->world_w = world_w;
    s->world_h = world_h;
    s->world_size = world_size;
    copy_input(s, input_size, input);
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
    fprintf(stderr, "world_size  = (%ld, %ld)\n", s->world_w, s->world_h);
    fprintf(stderr, "robot_point = (%ld, %ld)\n", s->robot_x, s->robot_y);
    fprintf(stderr, "lift_point  = (%ld, %ld)\n", s->lift_x, s->lift_y);
    fprintf(stderr, "move_count  = %ld\n", s->move_count);
    fprintf(stderr, "condition   = %d\n", s->condition);
    fprintf(stderr, "world_size  = %ld\n\n", s->world_size);
    fputs(s->world, stderr);
    fputc('\n', stderr);
}


struct state *copy(const struct state *s0) {
    DEBUG_ASSERT(s0);
    struct state *s;
    if (!(s = malloc(sizeof(struct state) + s0->world_size)))
        PERROR_EXIT("malloc");
    memcpy(s, s0, sizeof(struct state) + s0->world_size);
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


long get_move_count(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->move_count;
}


char get_condition(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->condition;
}


inline char lookup_point(const struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    long w, h, i;
    w = x - 1;
    DEBUG_ASSERT(w >= 0 && w < s->world_w);
    h = s->world_h - y;
    DEBUG_ASSERT(h >= 0 && h < s->world_h);
    i = h * (s->world_w + 1) + w;
    DEBUG_ASSERT(i < s->world_size);
    return s->world[i];
}


static void unsafe_make_one_move(struct state *s, char move) {
    DEBUG_ASSERT(s);
    DEBUG_ASSERT(s->condition == C_NONE);
    DEBUG_ASSERT(move == M_LEFT || move == M_RIGHT || move == M_UP || move == M_DOWN || move == M_WAIT || move == M_ABORT);
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
        object = lookup_point(s, x, y);
        // TODO
    } else if (move == M_ABORT)
        s->condition = C_ABORT;
    s->move_count++;
}


struct state *make_one_move(const struct state *s0, char move) {
    DEBUG_ASSERT(s0);
    struct state *s;
    s = copy(s0);
    unsafe_make_one_move(s, move);
    return s;
}


struct state *make_moves(const struct state *s0, const char *moves) {
    DEBUG_ASSERT(s0 && moves);
    struct state *s;
    s = copy(s0);
    while (*moves) {
        unsafe_make_one_move(s, *moves);
        moves++;
    }
    return s;
}
