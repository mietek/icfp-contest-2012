#!/usr/bin/env python -u
import subprocess, sys, os
from random import choice

validators = set(['v1', 'v2'])

authors = {'v1':'mietek',
           'v2':'pobara'}

def testFiles():
    dirname, dirnames, filenames = next(os.walk('tests/'))
    return filenames

def randomAnswer():
    return "LLLRRRUUUDDD" #TODO

def randomMapFile():
    return choice(testFiles())

def validatorOutput(validatorName, mapFile, answer):
    output = subprocess.Popen(["./" + validatorName, "-vv", mapFile, answer], stdout=subprocess.PIPE).communicate()[0].rstrip()
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
                print m1
                print
                print authors[v2] + ':'
                print m2
                mismatch = True

            if mismatch:
                print "on mapfile: " + mapFile
                print "on answer: " + answer
                print "=" * 80

        checkedValidators.add(v1)

    dotter += 1
    dotter %= 100
    if dotter == 0:
        print '.',
        sys.stdout.flush()

