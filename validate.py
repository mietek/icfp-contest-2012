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



from copy import *

def update_state(cmap_new):

    crushed = False

    def is_robot_crushed(rock_pos):
        global pos
        if pos == rock_pos - width:
            crushed = True

    if lambdas_to_lift == 0:
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

            # rock falling down

            if cmap[p_up]     == '*' and \
               cmap[p]        == ' ' :
               cmap_new[p_up] =  ' '
               cmap_new[p]    =  '*'
               is_robot_crushed(p)

            # rock falling right

            elif cmap[p_up] == '*'   and cmap[p_ur] == ' ' and \
                 cmap[p]    in '*\\' and cmap[p_r]  == ' ' :
                 cmap_new[p_up] = ' '
                 cmap_new[p_r]  = '*'
                 is_robot_crushed(p_r)

            # rock falling left

            elif cmap[p_ul] == ' ' and cmap[p_up] == '*' and \
                 cmap[p_l]  == ' ' and cmap[p]    == '*' and \
                 (cmap[p_ur] != ' ' or cmap[p_r] != ' ') :
                 cmap_new[p_up] = ' '
                 cmap_new[p_l]  = '*'
                 is_robot_crushed(p_r)

    return cmap_new, crushed

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
    crushed = False
    for move in moves:
        score -= 1
        move_f[move]()
        cmap, crushed = update_state(copy(cmap))
        if crushed: break
        #print_score()
        #pprint()
    if crushed:
        final_score = score + (lambdas_initial - lambdas_to_lift) * 25
    else:
        final_score = score + (lambdas_initial - lambdas_to_lift) * 50
except Escaped:
    #print "escaped"
    final_score = score + lambdas_initial * 75
except Aborted:
    #print "aborted"
    final_score = score + (lambdas_initial - lambdas_to_lift) * 50

print final_score
pprint()

