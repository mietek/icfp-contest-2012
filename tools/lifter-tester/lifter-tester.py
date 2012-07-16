#!/usr/bin/env python
import subprocess, sys, os, signal
import thread
import time

def testFiles():
    dirname, dirnames, filenames = next(os.walk('../../tests/'))
    return map (lambda s: '../../tests/' + s, filenames)

def validatorOutput(validatorName, mapFile, answer):
    p = subprocess.Popen(["./" + validatorName, "-vv", mapFile], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    output = p.communicate(input=answer+'\n')[0].rstrip()
    lines = output.split('\n')
    score = lines[0]
    finalMap = lines[1:]
    finalMap = map (lambda line: line.rstrip(), finalMap)
    while finalMap[-1] == '':
      finalMap = finalMap[0:-2]
    return (score, finalMap)

def retardedKiller(pid):
    time.sleep(150)
    os.kill(pid, signal.SIGINT)
    time.sleep(10)
    os.kill(pid, signal.SIGKILL)

lifter = None
mapfile = None
nruns = 10
validator = "../../bin/validator"

if len(sys.argv) < 3:
    print "wrong format"
    print "use lifter mapfile [nruns [validator]]"
    sys.exit()

elif len(sys.argv) >= 3:
    lifter = sys.argv[1]
    mapfile = sys.argv[2]

if len(sys.argv) >= 4:
    nruns = int(sys.argv[3])

if len(sys.argv) >= 5:
    validator = sys.argv[4]

logs = []

for n in range(nruns):
    p = subprocess.Popen(["cat " + mapfile + " | " + lifter], stdin=subprocess.PIPE, stdout=subprocess.PIPE, shell=True)
    pid = p.pid

    for i in range(150/15):
        time.sleep(15)
        print (i+1) * (150/15), "%",
        sys.stdout.flush()

    try:
        os.kill(pid, signal.SIGINT)
    except OSError:
        pass

    print "SIGINT",
    sys.stdout.flush()

    time.sleep(10)

    try:
        os.kill(pid, signal.SIGKILL)
    except OSError:
        pass

    print "SIGKILL"
    sys.stdout.flush()

    answer = p.communicate()[0].rstrip()
    score, finalmap = validatorOutput(validator, mapfile, answer)
    logs.append((score, answer))

for score, answer in sorted(logs):
    print score, answer
