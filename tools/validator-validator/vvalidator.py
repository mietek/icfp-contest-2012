#!/usr/bin/env python
import subprocess, sys, os
from random import choice

validators = set(['validate.py', 'lambdamine'])

authors = {'validate.py':'pobara',
           'lambdamine':'divide'}

moves = ['L', 'R', 'U', 'D', 'W']

def testFiles():
    dirname, dirnames, filenames = next(os.walk('../../tests/'))
    return map (lambda s: '../../tests/' + s, filenames)

def randomAnswer():
    ans = ''
    for i in range(choice(range(300))):
        ans += choice(moves)

    if choice(range(5)) == 1:
        ans += 'A'

    return ans

def randomMapFile():
    return choice(testFiles())

def validatorOutput(validatorName, mapFile, answer):
    p = subprocess.Popen(["./" + validatorName, "-vv", mapFile], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    output = p.communicate(input=answer+'\n')[0].rstrip()
    lines = output.split('\n')
    score = lines[0]
    finalMap = lines[1:]
    return (score, finalMap)

dotter = 0

while True:
    answer = randomAnswer()
    mapFile = randomMapFile()
    checkedValidators = set()
    for v1 in validators:
        for v2 in validators - set([v1]) - checkedValidators:
            s1, m1 = validatorOutput(v1, mapFile, answer)
            s2, m2 = validatorOutput(v2, mapFile, answer)

            mismatch = False
            if s1 != s2:
                print
                print "Score difference " + authors[v1] + ':' + s1 + ' ' + authors[v2] + ':' + s2
                mismatch = True

            if m1 != m2:
                print
                print "Final map difference:"
                print authors[v1] + ':'
                print
                for r in m1:
                    print r
                print
                print authors[v2] + ':'
                print
                for r in m2:
                    print r
                mismatch = True

            if mismatch:
                print
                print "on mapfile: " + mapFile
                print "on answer: " + answer
                print "=" * 80

        checkedValidators.add(v1)

    dotter += 1
    dotter %= 100
    if dotter == 0:
        print '.',
        sys.stdout.flush()

