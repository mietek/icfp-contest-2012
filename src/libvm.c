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


// ---------------------------------------------------------------------------
// Public
// ---------------------------------------------------------------------------

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
    s->robot_waterproofing = DEFAULT_ROBOT_WATERPROOFING;
    s->condition = C_NONE;
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

struct state *copy(const struct state *s0) {
    DEBUG_ASSERT(s0);
    struct state *s;
    if (!(s = malloc(sizeof(struct state) + s0->world_length)))
        PERROR_EXIT("malloc");
    memcpy(s, s0, sizeof(struct state) + s0->world_length);
    return s;
}

bool equal(const struct state *s1, const struct state *s2) {
    DEBUG_ASSERT(s1 && s2);
    if (s1->world_length != s2->world_length)
        return false;
    return !memcmp(s1, s2, sizeof(struct state) + s1->world_length);
}


void dump(const struct state *s) {
    DEBUG_ASSERT(s);
    DEBUG_LOG("world_size                 = (%ld, %ld)\n", s->world_w, s->world_h);
    DEBUG_LOG("robot_point                = (%ld, %ld)\n", s->robot_x, s->robot_y);
    DEBUG_LOG("lift_point                 = (%ld, %ld)\n", s->lift_x, s->lift_y);
    DEBUG_LOG("water_level                = %ld\n", s->water_level);
    DEBUG_LOG("flooding_rate              = %ld\n", s->flooding_rate);
    DEBUG_LOG("robot_waterproofing        = %ld\n", s->robot_waterproofing);
    DEBUG_LOG("used_robot_waterproofing   = %ld\n", s->used_robot_waterproofing);
    DEBUG_LOG("lambda_count               = %ld\n", s->lambda_count);
    DEBUG_LOG("collected_lambda_count     = %ld\n", s->collected_lambda_count);
    DEBUG_LOG("trampoline_count           = %ld\n", s->trampoline_count);
#ifdef DEBUG
    char trampoline, target;
    long i, j;
    for (i = 1; i <= MAX_TRAMPOLINE_COUNT; i++) {
        if (is_valid_point(s->trampoline_x[i], s->trampoline_y[i])) {
            trampoline = index_to_trampoline(i);
            j = s->trampoline_index_to_target_index[i];
            target = index_to_target(j);
            DEBUG_LOG("trampoline '%c' (%ld, %ld) -> '%c' (%ld, %ld)\n", trampoline, s->trampoline_x[i], s->trampoline_y[i], target, s->target_x[j], s->target_y[j]);
        }
    }
#endif
    DEBUG_LOG("move_count                 = %ld\n", s->move_count);
    DEBUG_LOG("score                      = %ld\n", s->score);
    DEBUG_LOG("condition                  = '%c'\n", s->condition);
    DEBUG_LOG("world_length               = %ld\n", s->world_length);
    printf("%ld\n%s", s->score, s->world);
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

long get_water_level(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->water_level;
}

long get_flooding_rate(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->flooding_rate;
}

long get_robot_waterproofing(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->robot_waterproofing;
}

long get_used_robot_waterproofing(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->used_robot_waterproofing;
}

long get_lambda_count(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->lambda_count;
}

long get_collected_lambda_count(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->collected_lambda_count;
}

long get_trampoline_count(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->trampoline_count;
}

bool get_trampoline_point(const struct state *s, char trampoline, long *out_trampoline_x, long *out_trampoline_y) {
    DEBUG_ASSERT(s && is_valid_trampoline(trampoline) && out_trampoline_x && out_trampoline_y);
    int i = trampoline_to_index(trampoline);
    if (is_valid_point(s->trampoline_x[i], s->trampoline_y[i])) {
        *out_trampoline_x = s->trampoline_x[i];
        *out_trampoline_y = s->trampoline_y[i];
        return true;
    }
    return false;
}

bool get_target_point(const struct state *s, char target, long *out_target_x, long *out_target_y) {
    DEBUG_ASSERT(s && is_valid_target(target) && out_target_x && out_target_y);
    int i = target_to_index(target);
    if (is_valid_point(s->target_x[i], s->target_y[i])) {
        *out_target_x = s->target_x[i];
        *out_target_y = s->target_y[i];
        return true;
    }
    return false;
}

bool get_trampoline_target(const struct state *s, char trampoline, char *out_target) {
    DEBUG_ASSERT(s && is_valid_trampoline(trampoline));
    int i = trampoline_to_index(trampoline);
    if (is_valid_point(s->trampoline_x[i], s->trampoline_y[i])) {
        *out_target = index_to_target(s->trampoline_index_to_target_index[i]);
        return true;
    }
    return false;
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

char safe_get(const struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    if (x < 1 || x > s->world_w || y < 1 || y > s->world_h)
        return O_WALL;
    return s->world[point_to_index(s, x, y)];
}


struct state *make_one_move(const struct state *s0, char move) {
    DEBUG_ASSERT(s0);
    struct state *s;
    s = copy(s0);
    if (s->condition == C_NONE && is_valid_move(move)) {
        execute_move(s, move);
        if (s->condition == C_NONE) {
            struct state *s1;
            s1 = copy(s);
            update_world(s1, s, DO_NOT_IGNORE_ROBOT);
            free(s);
            s = s1;
        }
    }
    return s;
}

struct state *make_moves(const struct state *s0, const char *moves) {
    DEBUG_ASSERT(s0 && moves);
    struct state *s;
    s = copy(s0);
    while (s->condition == C_NONE && is_valid_move(*moves)) {
        execute_move(s, *moves);
        if (s->condition == C_NONE) {
            struct state *s1;
            s1 = copy(s);
            update_world(s1, s, DO_NOT_IGNORE_ROBOT);
            free(s);
            s = s1;
        }
        moves++;
    }
    return s;
}


struct state *update_world_ignoring_robot(const struct state *s0) {
    DEBUG_ASSERT(s0);
    struct state *s;
    s = copy(s0);
    update_world(s, s0, IGNORE_ROBOT);
    return s;
}

struct state *imagine_robot_at(const struct state *s0, long x, long y) {
    DEBUG_ASSERT(s0);
    struct state *s;
    s = copy(s0);
    teleport_robot(s, x, y);
    return s;
}


bool is_enterable(const struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    char object;
    if (!is_within_world(s, x, y))
        return false;
    object = get(s, x, y);
    return object == O_EMPTY || object == O_EARTH || object == O_LAMBDA || object == O_OPEN_LIFT || is_valid_trampoline(object);
}


// ---------------------------------------------------------------------------
// Private
// ---------------------------------------------------------------------------

void scan_input(long input_length, const char *input, long *out_world_w, long *out_world_h, long *out_world_length) {
    DEBUG_ASSERT(input && out_world_w && out_world_h);
    long world_w = 0, world_h = 0, w = 0, i;
    for (i = 0; i < input_length; i++) {
        if (input[i] != '\n')
            w++;
        if (i == input_length - 1 || input[i] == '\n') {
            if (w == 0)
                break;
            if (w > world_w)
                world_w = w;
            world_h++;
            w = 0;
        }
    }
    *out_world_w = world_w;
    *out_world_h = world_h;
    *out_world_length = (world_w + 1) * world_h + 1;
}


enum {
    K_NONE,
    K_WATER_LEVEL,
    K_FLOODING_RATE,
    K_ROBOT_WATERPROOFING,
    K_TRAMPOLINE,
    K_TRAMPOLINE_TARGET_KEYWORD,
    K_TRAMPOLINE_TARGET,
    K_INVALID
};

#define MAX_TOKEN_SIZE 16

void copy_input_metadata(struct state *s, long input_length, const char *input) {
    DEBUG_ASSERT(s && input);
    char key = K_NONE, token[MAX_TOKEN_SIZE + 1];
    long w = 0, i, trampoline_i, target_i;
    for (i = 0; i < input_length; i++) {
        if (input[i] != ' ' && input[i] != '\n')
            w++;
        if (i == input_length - 1 || input[i] == ' ' || input[i] == '\n') {
            if (w == 0)
                continue;
            if (w > MAX_TOKEN_SIZE)
                w = MAX_TOKEN_SIZE;
            memcpy(token, input + i - w, w);
            token[w] = 0;
            if (key == K_NONE) {
                if (!strcmp(token, "Water"))
                    key = K_WATER_LEVEL;
                else if (!strcmp(token, "Flooding"))
                    key = K_FLOODING_RATE;
                else if (!strcmp(token, "Waterproof"))
                    key = K_ROBOT_WATERPROOFING;
                else if (!strcmp(token, "Trampoline"))
                    key = K_TRAMPOLINE;
                else {
                    key = K_INVALID;
                    DEBUG_LOG("found invalid metadata key '%s'\n", token);
                }
            } else {
                if (key == K_WATER_LEVEL) {
                    s->water_level = atoi(token);
                    key = K_NONE;
                } else if (key == K_FLOODING_RATE) {
                    s->flooding_rate = atoi(token);
                    key = K_NONE;
                } else if (key == K_ROBOT_WATERPROOFING) {
                    s->robot_waterproofing = atoi(token);
                    key = K_NONE;
                } else if (key == K_TRAMPOLINE) {
                    trampoline_i = trampoline_to_index(*token);
                    key = K_TRAMPOLINE_TARGET_KEYWORD;
                } else if (key == K_TRAMPOLINE_TARGET_KEYWORD)
                    key = K_TRAMPOLINE_TARGET;
                else if (key == K_TRAMPOLINE_TARGET) {
                    target_i = target_to_index(*token);
                    s->trampoline_index_to_target_index[trampoline_i] = target_i;
                    s->trampoline_count++;
                    key = K_NONE;
                } else {
                    key = K_NONE;
                    DEBUG_LOG("found invalid metadata value '%s'\n", token);
                }
            }
            w = 0;
        }
    }
}

void copy_input(struct state *s, long input_length, const char *input) {
    DEBUG_ASSERT(s && input);
    long w = 0, h = 0, j = 0, i, trampoline_i, target_i;
    for (i = 0; i < input_length; i++) {
        if (input[i] != '\n') {
            if (input[i] == O_ROBOT)
                size_to_point(s, w, h, &s->robot_x, &s->robot_y);
            else if (input[i] == O_LAMBDA)
                s->lambda_count++;
            else if (input[i] == O_CLOSED_LIFT)
                size_to_point(s, w, h, &s->lift_x, &s->lift_y);
            else if (is_valid_trampoline(input[i])) {
                trampoline_i = trampoline_to_index(input[i]);
                size_to_point(s, w, h, &s->trampoline_x[trampoline_i], &s->trampoline_y[trampoline_i]);
            } else if (is_valid_target(input[i])) {
                target_i = target_to_index(input[i]);
                size_to_point(s, w, h, &s->target_x[target_i], &s->target_y[target_i]);
            }
            s->world[j] = input[i];
            // TODO:
            if (input[i] == O_BEARD)
                s->world[j] = O_WALL;
            else if (input[i] == O_RAZOR)
                s->world[j] = O_EARTH;
            j++;
            w++;
        }
        if (i == input_length - 1 || input[i] == '\n') {
            if (w == 0)
                break;
            while (w < s->world_w) {
                s->world[j++] = O_EMPTY;
                w++;
            }
            s->world[j] = '\n';
            j++;
            h++;
            w = 0;
        }
    }
    i++;
    s->world[j] = 0;
    copy_input_metadata(s, input_length - i, input + i);
}


void teleport_robot(struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    put(s, s->robot_x, s->robot_y, O_EMPTY);
    s->robot_x = x;
    s->robot_y = y;
    put(s, x, y, O_ROBOT);
}

void move_robot(struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    DEBUG_ASSERT(is_enterable(s, x, y) || (get(s, x, y) == O_ROCK && get(s, x + x - s->robot_x, y) == O_EMPTY));
    char object;
    long trampoline_i, target_i;
    object = get(s, x, y);
    if (is_valid_trampoline(object)) {
        trampoline_i = trampoline_to_index(object);
        DEBUG_ASSERT(s->trampoline_x[trampoline_i]);
        target_i = s->trampoline_index_to_target_index[trampoline_i];
        DEBUG_ASSERT(s->target_x[target_i]);
        teleport_robot(s, s->target_x[target_i], s->target_y[target_i]);
        DEBUG_LOG("robot trampolined to (%ld, %ld)\n", s->robot_x, s->robot_y);
    } else {
        teleport_robot(s, x, y);
        DEBUG_LOG("robot moved to (%ld, %ld)\n", s->robot_x, s->robot_y);
    }
    if (s->used_robot_waterproofing && s->robot_y > s->water_level) {
        s->used_robot_waterproofing = 0;
        DEBUG_LOG("robot waterproofing restored\n");
    }
}

void collect_lambda(struct state *s) {
    DEBUG_ASSERT(s);
    DEBUG_ASSERT(s->collected_lambda_count < s->lambda_count);
    DEBUG_ASSERT(get(s, s->lift_x, s->lift_y) == O_CLOSED_LIFT);
    s->collected_lambda_count++;
    s->score += 25;
    DEBUG_LOG("robot collected lambda\n");
}

void clear_similar_trampolines(struct state *s, char trampoline) {
    DEBUG_ASSERT(s && is_valid_trampoline(trampoline));
    long trampoline_i, target_i, i;
    trampoline_i = trampoline_to_index(trampoline);
    DEBUG_ASSERT(s->trampoline_x[trampoline_i]);
    target_i = s->trampoline_index_to_target_index[trampoline_i];
    DEBUG_ASSERT(s->target_x[target_i]);
    for (i = 1; i <= MAX_TRAMPOLINE_COUNT; i++) {
        if (s->trampoline_index_to_target_index[i] == target_i) {
            put(s, s->trampoline_x[i], s->trampoline_y[i], O_EMPTY);
            s->trampoline_x[i] = 0;
            s->trampoline_y[i] = 0;
            s->trampoline_index_to_target_index[i] = 0;
            s->trampoline_count--;
            DEBUG_LOG("robot cleared trampoline '%c'\n", trampoline);
        }
    }
}

void execute_move(struct state *s, char move) {
    DEBUG_ASSERT(s && is_valid_move(move));
    DEBUG_ASSERT(s->condition == C_NONE);
    if (move == M_LEFT || move == M_RIGHT || move == M_UP || move == M_DOWN) {
        long x, y;
        char object;
        x = s->robot_x;
        y = s->robot_y;
        if (move == M_LEFT)
            x = s->robot_x - 1;
        else if (move == M_RIGHT)
            x = s->robot_x + 1;
        else if (move == M_UP)
            y = s->robot_y + 1;
        else if (move == M_DOWN)
            y = s->robot_y - 1;
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
            DEBUG_LOG("robot won\n");
        } else if (object == O_ROCK && move == M_LEFT && get(s, x - 1, y) == O_EMPTY) {
            move_robot(s, x, y);
            put(s, x - 1, y, O_ROCK);
            DEBUG_LOG("robot pushed rock from (%ld, %ld) to (%ld, %ld)\n", x, y, x - 1, y);
        } else if (object == O_ROCK && move == M_RIGHT && get(s, x + 1, y) == O_EMPTY) {
            move_robot(s, x, y);
            put(s, x + 1, y, O_ROCK);
            DEBUG_LOG("robot pushed rock from (%ld, %ld) to (%ld, %ld\n", x, y, x + 1, y);
        } else if (is_valid_trampoline(object)) {
            move_robot(s, x, y);
            clear_similar_trampolines(s, object);
        } else
            DEBUG_LOG("robot attempted invalid move '%c' from (%ld, %ld) to (%ld, %ld) which is '%c'\n", move, s->robot_x, s->robot_y, x, y, object);
        s->move_count++;
        s->score--;
    } else if (move == M_WAIT) {
        DEBUG_LOG("robot waited\n");
        s->move_count++;
        s->score--;
    } else if (move == M_ABORT) {
        s->score += s->collected_lambda_count * 25;
        s->condition = C_ABORT;
        DEBUG_LOG("robot aborted\n");
    }
}

void update_world(struct state *s, const struct state *s0, bool ignore_robot) {
    DEBUG_ASSERT(s && s0);
    DEBUG_ASSERT(s->condition == C_NONE);
    long x, y;
    for (y = 1; y <= s->world_h; y++) {
        for (x = 1; x <= s->world_w; x++) {
            char object;
            object = get(s0, x, y);
            if (object == O_ROCK && get(s0, x, y - 1) == O_EMPTY) {
                put(s, x, y, O_EMPTY);
                put(s, x, y - 1, O_ROCK);
                if (!ignore_robot && s0->robot_x == x && s0->robot_y == y - 2) {
                    s->condition = C_LOSE;
                    DEBUG_LOG("robot lost by crushing\n");
                }
            } else if (object == O_ROCK && get(s0, x, y - 1) == O_ROCK && get(s0, x + 1, y) == O_EMPTY && get(s0, x + 1, y - 1) == O_EMPTY) {
                put(s, x, y, O_EMPTY);
                put(s, x + 1, y - 1, O_ROCK);
                if (!ignore_robot && s0->robot_x == x + 1 && s0->robot_y == y - 2) {
                    s->condition = C_LOSE;
                    DEBUG_LOG("robot lost by crushing\n");
                }
            } else if (object == O_ROCK && get(s0, x, y - 1) == O_ROCK && (get(s0, x + 1, y) != O_EMPTY || get(s0, x + 1, y - 1) != O_EMPTY) && get(s0, x - 1, y) == O_EMPTY && get(s0, x - 1, y - 1) == O_EMPTY) {
                put(s, x, y, O_EMPTY);
                put(s, x - 1, y - 1, O_ROCK);
                if (!ignore_robot && s0->robot_x == x - 1 && s0->robot_y == y - 2) {
                    s->condition = C_LOSE;
                    DEBUG_LOG("robot lost by crushing\n");
                }
            } else if (object == O_ROCK && get(s0, x, y - 1) == O_LAMBDA && get(s0, x + 1, y) == O_EMPTY && get(s0, x + 1, y - 1) == O_EMPTY) {
                put(s, x, y, O_EMPTY);
                put(s, x + 1, y - 1, O_ROCK);
                if (!ignore_robot && s0->robot_x == x + 1 && s0->robot_y == y - 2) {
                    s->condition = C_LOSE;
                    DEBUG_LOG("robot lost by crushing\n");
                }
            } else if (!ignore_robot && object == O_CLOSED_LIFT && s0->collected_lambda_count == s0->lambda_count) {
                put(s, x, y, O_OPEN_LIFT);
                DEBUG_LOG("robot opened lift\n");
            }
        }
    }
    if (!ignore_robot && s0->robot_y <= s->water_level) {
        DEBUG_LOG("robot is underwater\n");
        s->used_robot_waterproofing++;
        if (s->used_robot_waterproofing > s->robot_waterproofing) {
            s->condition = C_LOSE;
            DEBUG_LOG("robot lost by drowning\n");
        }
    }
    if (!ignore_robot && s->flooding_rate && !(s->move_count % s->flooding_rate)) {
        s->water_level++;
        DEBUG_LOG("robot increased water level to %ld\n", s->water_level);
    }
}
