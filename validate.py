#!/bin/env python

from array import *
import sys

if len(sys.argv) <= 1:
    print 'usage example: $ echo "LLLRRRDDDA" | ./validate.py <map_file>'
    sys.exit(0)

def verbose():
    return '-v' in sys.argv

def get_map(path):
    map_file = open(path)
    n = len(map_file.readline()) - 1
    m = 0
    map_file.seek(0)
    cmap = array('c')
    while True:
        line = map_file.readline().strip()
        if not line: break
        m += 1
        cmap.fromstring(line)
    return (n, m), cmap

size, cmap = get_map(sys.argv[1])
width, height = size
pos = cmap.index('R')
lambdas_to_lift = cmap.count('\\')
lambdas_initial = lambdas_to_lift
reachable_tiles = " .\\O"
score = 0
final_score = 0

class Crushed(Exception): pass
class Escaped(Exception): pass
class Aborted(Exception): pass

def pprint():
    for i in range(height):
        row = i*width
        print cmap[row:row+width].tostring()
    print

def print_score():
    if not verbose(): return
    print "%d" % (score + (lambdas_initial - lambdas_to_lift) * 25)

def is_left(blocks):
    bm = array('c', blocks)
    return cmap[pos-len(blocks):pos] == bm

def is_right(blocks):
    bm = array('c', blocks)
    return cmap[pos:len(blocks)+pos] == bm

def set_left(blocks):
    p = pos
    for c in reversed(blocks):
        cmap[p] = c
        p -= 1

def set_right(blocks):
    p = pos
    for c in blocks:
        p += 1
        cmap[p] = c

def move_left():
    global pos
    global lambdas_to_lift
    global score
    x = cmap[pos-1]
    if x in reachable_tiles:
        if x == '\\': lambdas_to_lift -= 1
        if x == 'O':  raise Escaped
        cmap[pos] = ' '
        pos -= 1
        cmap[pos] = 'R'
    elif is_left(' *'):
        cmap[pos] = ' '
        pos -= 1
        set_left('*R')

def move_right():
    global pos
    global lambdas_to_lift
    x = cmap[pos+1]
    if x in reachable_tiles:
        if x == '\\': lambdas_to_lift -= 1
        if x == 'O':  raise Escaped
        cmap[pos] = ' '
        pos += 1
        cmap[pos] = 'R'
    elif is_right('* '):
        cmap[pos] = ' '
        pos += 1
        set_right('R*')

def move_up():
    global pos
    global lambdas_to_lift
    x = cmap[pos-width]
    if x in reachable_tiles:
        if x == '\\': lambdas_to_lift -= 1
        if x == 'O':  raise Escaped
        cmap[pos] = ' '
        pos -= width
        cmap[pos] = 'R'

def move_down():
    global pos
    global lambdas_to_lift
    x = cmap[pos+width]
    if x in reachable_tiles:
        if x == '\\': lambdas_to_lift -= 1
        if x == 'O':  raise Escaped
        cmap[pos] = ' '
        pos += width
        cmap[pos] = 'R'

def is_robot_crushed(rock_pos):
    if pos == rock_pos - width:
        raise Crushed

def update_state():

    if lambdas_to_lift == 0:
        # assuming one lift for now
        try:
            pos = cmap.index('L')
            cmap[pos] = 'O'
        except: pass

    for i in reversed(range(height)):
        for j in range(1,width-1):
            p    = i*width+j
            p_up = p-width
            p_r  = p+1
            p_ur = p_up+1
            p_l  = p-1
            p_ul = p_up-1

            tile    = cmap[p]
            tile_up = cmap[p_up]
            tile_r  = cmap[p_r]
            tile_ur = cmap[p_ur]
            tile_l  = cmap[p_l]
            tile_ul = cmap[p_ul]

            # rock falling down

            if tile == ' ' and tile_up == '*' :
               tile, tile_up = tile_up, tile
               is_robot_crushed(p)

            # rock falling right

            elif tile_up == '*'   and tile_ur == ' ' and \
                 tile    in '*\\' and tile_r  == ' ' :
               tile_up, tile_r = tile_r, tile_up
               is_robot_crushed(p_r)

            # rock falling left

            elif tile_ul == ' ' and tile_up == '*' and \
                 tile_l  == ' ' and tile    == '*' and \
                 (tile_ur != ' ' or tile_r != ' ') :
               tile_up, tile_l = tile_l, tile_up
               is_robot_crushed(p_l)

            cmap[p]    = tile
            cmap[p_up] = tile_up
            cmap[p_r]  = tile_r
            cmap[p_ur] = tile_ur
            cmap[p_l]  = tile_l
            cmap[p_ul] = tile_ul

def abort():
    raise Aborted

def pass_f(): pass
move_f = {}
move_f['L'] = move_left
move_f['R'] = move_right
move_f['U'] = move_up
move_f['D'] = move_down
move_f['W'] = pass_f
move_f['A'] = abort

#pprint()
final_score = 0
moves = raw_input('')

try:
    for move in moves:
        score -= 1
        move_f[move]()
        update_state()
        #print_score()
        #pprint()
    final_score = score + (lambdas_initial - lambdas_to_lift) * 25
except Crushed:
    #print "crushed"
    final_score = score + (lambdas_initial - lambdas_to_lift) * 25
except Escaped:
    #print "escaped"
    final_score = score + lambdas_initial * 75
except Aborted:
    #print "aborted"
    final_score = score + (lambdas_initial - lambdas_to_lift) * 50

print final_score
pprint()

