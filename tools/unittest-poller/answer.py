#!/usr/bin/env python
import subprocess, sys, os
from random import choice, random

moves = ['L', 'R', 'U', 'D', 'W']

SHAVE_CHANCE = 0.02

def randomAnswer():
    ans = ''
    for i in range(choice(range(150))):
      if random() < SHAVE_CHANCE:
        ans += 'S'
      else:
        ans += choice(moves)

    if choice(range(5)) == 1:
        ans += 'A'

    return ans

print randomAnswer()
