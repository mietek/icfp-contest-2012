#include <assert.h>
#include <fcntl.h>
#include <limits.h>
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
    scan_input(input_length, input, &world_w, &world_h);
    world_length = (world_w + 1) * world_h + 1;
    if (!(s = malloc(sizeof(struct state) + world_length)))
        PERROR_EXIT("malloc");
    memset(s, 0, sizeof(struct state));
    s->world_w = world_w;
    s->world_h = world_h;
    s->robot_waterproofing = DEFAULT_ROBOT_WATERPROOFING;
    s->beard_growth_rate = DEFAULT_BEARD_GROWTH_RATE;
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
    DEBUG_LOG("beard_growth_rate          = %ld\n", s->beard_growth_rate);
    DEBUG_LOG("razor_count                = %ld\n", s->razor_count);
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

long get_beard_growth_rate(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->beard_growth_rate;
}

long get_razor_count(const struct state *s) {
    DEBUG_ASSERT(s);
    return s->razor_count;
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
    long i = trampoline_to_index(trampoline);
    if (is_valid_point(s->trampoline_x[i], s->trampoline_y[i])) {
        *out_trampoline_x = s->trampoline_x[i];
        *out_trampoline_y = s->trampoline_y[i];
        return true;
    }
    return false;
}

bool get_target_point(const struct state *s, char target, long *out_target_x, long *out_target_y) {
    DEBUG_ASSERT(s && is_valid_target(target) && out_target_x && out_target_y);
    long i = target_to_index(target);
    if (is_valid_point(s->target_x[i], s->target_y[i])) {
        *out_target_x = s->target_x[i];
        *out_target_y = s->target_y[i];
        return true;
    }
    return false;
}

bool get_trampoline_target(const struct state *s, char trampoline, char *out_target) {
    DEBUG_ASSERT(s && is_valid_trampoline(trampoline));
    long i = trampoline_to_index(trampoline);
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
    if (!is_within_world(s->world_w, s->world_h, x, y))
        return O_WALL;
    return get(s, x, y);
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
    s->move_count++;
    s->score--;
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

void get_step(const struct state *s, char move, long *out_x, long *out_y) {
    DEBUG_ASSERT(s && is_valid_move(move) && out_x && out_y);
    struct state *s1;
    s1 = make_one_move(s, move);
    get_robot_point(s1, out_x, out_y);
    free(s1);
}

void imagine_step(const struct state *s, long x, long y, char move, long *out_x, long *out_y) {
    DEBUG_ASSERT(s && is_valid_move(move) && out_x && out_y);
    struct state *s1;
    s1 = imagine_robot_at(s, x, y);
    get_step(s1, move, out_x, out_y);
    free(s1);
}


bool is_enterable(const struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    char object;
    if (!is_within_world(s->world_w, s->world_h, x, y))
        return false;
    object = get(s, x, y);
    return object == O_EMPTY || object == O_EARTH || object == O_LAMBDA || object == O_RAZOR || object == O_LIFT_OPEN || is_valid_trampoline(object);
}

bool is_safe(const struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    struct state *s1;
    bool safe;
    if (!is_enterable(s, x, y))
        safe = false;
    else if (get(s, x, y + 1) == O_EMPTY || get(s, x, y + 1) == O_ROBOT) {
        s1 = update_world_ignoring_robot(s);
        safe = !is_rock_object(get(s1, x, y + 1));
        free(s1);
    } else
        safe = true;
    return safe;
}


struct cost_table *build_cost_table(const struct state *s, long x, long y) {
    DEBUG_ASSERT(s && is_within_world(s->world_w, s->world_h, x, y));
    long world_length, i;
    struct cost_table *ct;
    world_length = s->world_w * s->world_h;
    if (!(ct = malloc(sizeof(struct cost_table) + (2 * world_length * sizeof(long)))))
        PERROR_EXIT("malloc");
    memset(ct, 0, sizeof(struct cost_table));
    ct->world_w = s->world_w;
    ct->world_h = s->world_h;
    ct->world_length = world_length;
    for (i = 0; i < world_length; i++) {
        ct->world_cost[i] = MAX_COST;
        ct->world_cost[ct->world_length+i] = MAX_COST;
    }
    put_cost(ct, x, y, 0);
    put_dist(ct, x, y, 0);
    run_dijkstra(ct, s);
    return ct;
}

long safe_get_cost(const struct cost_table *ct, long x, long y) {
    DEBUG_ASSERT(ct);
    if (!is_within_world(ct->world_w, ct->world_h, x, y))
        return MAX_COST;
    return get_cost(ct, x, y);
}

long safe_get_dist(const struct cost_table *ct, long x, long y) {
    DEBUG_ASSERT(ct);
    if (!is_within_world(ct->world_w, ct->world_h, x, y))
        return MAX_COST;
    return get_dist(ct, x, y);
}


// ---------------------------------------------------------------------------
// Private
// ---------------------------------------------------------------------------

void scan_input(long input_length, const char *input, long *out_world_w, long *out_world_h) {
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
}


enum {
    K_NONE,
    K_WATER_LEVEL,
    K_FLOODING_RATE,
    K_ROBOT_WATERPROOFING,
    K_TRAMPOLINE,
    K_TRAMPOLINE_TARGET,
    K_TARGET,
    K_BEARD_GROWTH_RATE,
    K_RAZOR_COUNT,
    K_INVALID
};

#define MAX_TOKEN_SIZE 16

void copy_input_metadata(struct state *s, long input_length, const char *input) {
    DEBUG_ASSERT(s && input);
    char key = K_NONE, token[MAX_TOKEN_SIZE + 1];
    long w = 0, i, trampoline_i, target_i;
    trampoline_i = 0;
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
                else if (!strcmp(token, "Growth"))
                    key = K_BEARD_GROWTH_RATE;
                else if (!strcmp(token, "Razors"))
                    key = K_RAZOR_COUNT;
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
                } else if (key == K_BEARD_GROWTH_RATE) {
                    s->beard_growth_rate = atoi(token);
                    key = K_NONE;
                } else if (key == K_RAZOR_COUNT) {
                    s->razor_count = atoi(token);
                    key = K_NONE;
                } else if (key == K_TRAMPOLINE) {
                    trampoline_i = trampoline_to_index(*token);
                    key = K_TRAMPOLINE_TARGET;
                } else if (key == K_TRAMPOLINE_TARGET)
                    key = K_TARGET;
                else if (key == K_TARGET) {
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
                size_to_point(s->world_h, w, h, &s->robot_x, &s->robot_y);
            else if (input[i] == O_LAMBDA || input[i] == O_HO_ROCK)
                s->lambda_count++;
            else if (input[i] == O_LIFT_CLOSED)
                size_to_point(s->world_h, w, h, &s->lift_x, &s->lift_y);
            else if (is_valid_trampoline(input[i])) {
                trampoline_i = trampoline_to_index(input[i]);
                size_to_point(s->world_h, w, h, &s->trampoline_x[trampoline_i], &s->trampoline_y[trampoline_i]);
            } else if (is_valid_target(input[i])) {
                target_i = target_to_index(input[i]);
                size_to_point(s->world_h, w, h, &s->target_x[target_i], &s->target_y[target_i]);
            }
            s->world[j] = input[i];
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
    DEBUG_ASSERT(is_enterable(s, x, y) || (is_rock_object(get(s, x, y)) && get(s, x + x - s->robot_x, y) == O_EMPTY));
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
    DEBUG_ASSERT(get(s, s->lift_x, s->lift_y) == O_LIFT_CLOSED);
    s->collected_lambda_count++;
    s->score += 50;
    DEBUG_LOG("robot collected lambda\n");
}

void collect_razor(struct state *s) {
    DEBUG_ASSERT(s);
    s->razor_count++;
    DEBUG_LOG("robot collected razor\n");
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
        } else if (object == O_RAZOR) {
            move_robot(s, x, y);
            collect_razor(s);
        } else if (object == O_LIFT_OPEN) {
            put(s, s->robot_x, s->robot_y, O_EMPTY);
            s->score += s->collected_lambda_count * 25;
            s->condition = C_WIN;
            DEBUG_LOG("robot won\n");
        } else if (object == O_ROCK && move == M_LEFT && get(s, x - 1, y) == O_EMPTY) {
            move_robot(s, x, y);
            put(s, x - 1, y, O_ROCK);
            DEBUG_LOG("robot pushed rock from (%ld, %ld) to (%ld, %ld)\n", x, y, x - 1, y);
        } else if (object == O_HO_ROCK && move == M_LEFT && get(s, x - 1, y) == O_EMPTY) {
            move_robot(s, x, y);
            put(s, x - 1, y, O_HO_ROCK);
            DEBUG_LOG("robot pushed higher order rock from (%ld, %ld) to (%ld, %ld)\n", x, y, x - 1, y);
        } else if (object == O_ROCK && move == M_RIGHT && get(s, x + 1, y) == O_EMPTY) {
            move_robot(s, x, y);
            put(s, x + 1, y, O_ROCK);
            DEBUG_LOG("robot pushed rock from (%ld, %ld) to (%ld, %ld)\n", x, y, x + 1, y);
        } else if (object == O_HO_ROCK && move == M_RIGHT && get(s, x + 1, y) == O_EMPTY) {
            move_robot(s, x, y);
            put(s, x + 1, y, O_HO_ROCK);
            DEBUG_LOG("robot pushed higher order rock from (%ld, %ld) to (%ld, %ld)\n", x, y, x + 1, y);
        } else if (is_valid_trampoline(object)) {
            move_robot(s, x, y);
            clear_similar_trampolines(s, object);
        } else
            DEBUG_LOG("robot attempted invalid move '%c' from (%ld, %ld) to (%ld, %ld) which is '%c'\n", move, s->robot_x, s->robot_y, x, y, object);
        s->move_count++;
        s->score--;
    } else if (move == M_SHAVE) {
        shave_beard(s, s->robot_x, s->robot_y);
        s->move_count++;
        s->score--;
    } else if (move == M_WAIT) {
        DEBUG_LOG("robot waited\n");
        s->move_count++;
        s->score--;
    } else if (move == M_ABORT) {
        s->condition = C_ABORT;
        DEBUG_LOG("robot aborted\n");
    }
}


void shave_beard(struct state *s, long x, long y) {
    DEBUG_ASSERT(s);
    long i, j;
    if (s->razor_count) {
        for (i = -1; i <= 1; i++) {
            for (j = -1; j <= 1; j++) {
                if (get(s, x + i, y + j) == O_BEARD) {
                    put(s, x + i, y + j, O_EMPTY);
                    DEBUG_LOG("robot shaved beard at (%ld, %ld)\n", x, y);
                }
            }
        }
        s->razor_count--;
    } else
        DEBUG_LOG("robot attempted to shave without a razor\n");
}

void grow_beard(struct state *s, const struct state *s0, long x, long y) {
    DEBUG_ASSERT(s && s0);
    long i, j;
    for (i = -1; i <= 1; i++) {
        for (j = -1; j <= 1; j++) {
            if (get(s0, x + i, y + j) == O_EMPTY) {
                put(s, x + i, y + j, O_BEARD);
                DEBUG_LOG("beard grew at (%ld, %ld)\n", x, y);
            }
        }
    }
}


void drop_rock(struct state *s, const struct state *s0, char rock, long x, long y, bool ignore_robot) {
    DEBUG_ASSERT(s && s0 && is_rock_object(rock));
    char below;
    below = get(s0, x, y - 1);
    if (!ignore_robot && below == O_ROBOT) {
        s->score -= s->collected_lambda_count * 25;
        s->condition = C_LOSE;
        DEBUG_LOG("robot lost by crushing\n");
    }
    if (below != O_EMPTY && rock == O_HO_ROCK) {
        put(s, x, y, O_LAMBDA);
        DEBUG_LOG("higher order rock turned into lambda at (%ld, %ld)\n", x, y);
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
            if (is_rock_object(object)) {
                char below;
                below = get(s0, x, y - 1);
                if (below == O_EMPTY) {
                    put(s, x, y, O_EMPTY);
                    put(s, x, y - 1, object);
                    drop_rock(s, s0, object, x, y - 1, ignore_robot);
                } else if (is_rock_object(below) && get(s0, x + 1, y) == O_EMPTY && get(s0, x + 1, y - 1) == O_EMPTY) {
                    put(s, x, y, O_EMPTY);
                    put(s, x + 1, y - 1, object);
                    drop_rock(s, s0, object, x + 1, y - 1, ignore_robot);
                } else if (is_rock_object(below) && get(s0, x - 1, y) == O_EMPTY && get(s0, x - 1, y - 1) == O_EMPTY) {
                    put(s, x, y, O_EMPTY);
                    put(s, x - 1, y - 1, object);
                    drop_rock(s, s0, object, x - 1, y - 1, ignore_robot);
                } else if (below == O_LAMBDA && get(s0, x + 1, y) == O_EMPTY && get(s0, x + 1, y - 1) == O_EMPTY) {
                    put(s, x, y, O_EMPTY);
                    put(s, x + 1, y - 1, object);
                    drop_rock(s, s0, object, x + 1, y - 1, ignore_robot);
                }
            } else if (object == O_BEARD && s->beard_growth_rate && !(s->move_count % s->beard_growth_rate))
                grow_beard(s, s0, x, y);
            else if (object == O_LIFT_CLOSED && s0->collected_lambda_count == s0->lambda_count) {
                put(s, x, y, O_LIFT_OPEN);
                DEBUG_LOG("lift opened\n");
            }
        }
    }
    if (!ignore_robot && s0->robot_y <= s->water_level) {
        DEBUG_LOG("robot is underwater\n");
        s->used_robot_waterproofing++;
        if (s->used_robot_waterproofing > s->robot_waterproofing) {
            s->score -= s->collected_lambda_count * 25;
            s->condition = C_LOSE;
            DEBUG_LOG("robot lost by drowning\n");
        }
    }
    if (s->flooding_rate && !(s->move_count % s->flooding_rate)) {
        s->water_level++;
        DEBUG_LOG("water level increased to %ld\n", s->water_level);
    }
    if (s->move_count == s->world_w * s->world_h) {
        s->condition = C_ABORT;
        DEBUG_LOG("move limit reached\n");
    }
}


long calculate_cost(const struct state *s, long step_x, long step_y, long stage) {
    if (safe_get(s, step_x, step_y) == O_LAMBDA)
        return 1;
    if (safe_get(s, step_x, step_y) == O_EMPTY)
        return 4;
    if (
        safe_get(s, step_x, step_y + 1) == O_ROCK ||
        (safe_get(s, step_x + 1, step_y + 1) == O_ROCK && safe_get(s, step_x + 1, step_y) == O_ROCK) ||
        (safe_get(s, step_x - 1, step_y + 1) == O_ROCK && safe_get(s, step_x - 1, step_y) == O_ROCK) ||
        safe_get(s, step_x, step_y + 1) == O_HO_ROCK ||
        (safe_get(s, step_x + 1, step_y + 1) == O_HO_ROCK && safe_get(s, step_x + 1, step_y) == O_HO_ROCK) ||
        (safe_get(s, step_x - 1, step_y + 1) == O_HO_ROCK && safe_get(s, step_x - 1, step_y) == O_HO_ROCK)
    )
        return 40;
    return 10;
}

void run_dijkstra(struct cost_table *ct, const struct state *s) {
    DEBUG_ASSERT(ct);
    long change, stage, step_x[4], step_y[4], i, j, k;
    struct state *s1, *s2;
    s1 = copy(s);
    change = 0;
    stage = 0;
    do {
        change++;
        for (i = 1; i <= ct->world_w; i++) {
            for (j = 1; j <= ct->world_h; j++) {
                if (get_dist(ct, i, j) == stage) {
                    // TODO: This is suboptimal.
                    imagine_step(s1, i, j, M_LEFT,  &step_x[0], &step_y[0]);
                    imagine_step(s1, i, j, M_RIGHT, &step_x[1], &step_y[1]);
                    imagine_step(s1, i, j, M_UP,    &step_x[2], &step_y[2]);
                    imagine_step(s1, i, j, M_DOWN,  &step_x[3], &step_y[3]);
                    for (k = 0; k < 4; k++) {
                        if (is_safe(s1, step_x[k], step_y[k]) && get_cost(ct, step_x[k], step_y[k]) > get_cost(ct, i, k) + calculate_cost(s1, step_x[k], step_y[k], stage)) {
                            put_cost(ct, step_x[k], step_y[k], get_cost(ct, i, k) + calculate_cost(s1, step_x[k], step_y[k], stage));
                            put_dist(ct, step_x[k], step_y[k], stage + 1);
                            change = 0;
                        }
                    }
                }
            }
        }
        stage++;
        s2 = update_world_ignoring_robot(s1);
        free(s1);
        s1 = s2;
    } while (!change);
    free(s1);
}
