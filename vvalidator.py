#!/usr/bin/env python
import subprocess

validators = set([{'exec':'v1','author':'mietek'},
                  {'exec':'v2','author':'pobara'}])

def randomAnswer():
    return "LLLRRRUUUDDD" #TODO

def randomMapFile():
    return "tests/contest1.map" #TODO

def validatorOutput(validatorName, mapFile, answer):
    output = subprocess.Popen(["./" + validatorName, "-vv", mapFile, answer], stdout=subprocess.PIPE).communicate()[0].rstrip()
    lines = output.split('\n')
    score = lines[0]
    finalMap = lines[1:]
    return (score, finalMap)

while True:
    answer = randomAnswer()
    mapFile = randomMapFile()
    checkedValidators = set()
    for v1 in validators:
        for v2 in validators - set([v1]) - checkedValidators:
            s1, m1 = validatorOutput(v1['exec'], mapFile, answer)
            s2, m2 = validatorOutput(v1['exec'], mapFile, answer)

            mismatch = False
            if s1 != s2:
                print "Score difference " + v1['author'] + ':' + s1 + ' ' + v2['author'] + ':' + s2
                mismatch = True

            if m1 != m2:
                print "Final map difference " + v1['author'] + ':'
                print s1
                print
                print v2['author'] + ':'
                print s2
                mismatch = True

            if mismatch:
                print "on mapfile: " + mapFile
                print "on answer: " + answer

        checkedValidators.add(v1)

