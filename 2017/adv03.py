#!/usr/bin/env python3

import math

offt = 1000

data = [None] * (offt * 2)

for i in range(offt*2):
    data[i] = [0] * (offt * 2)

inp = 361527

side = 'd'
x = 0
y = 0
def dump():
    for dy in range(-2, 3):
        print(" ".join([str(data[offt+dx][offt+dy]) for dx in range(-2, 3)]))
    print()
data[offt][offt] = 1

#for _ in range(inp-1):
while True:
    #dump()
    if side == 'd':
        if x == y:
            x += 1
            side = 'r'
        else:
            x += 1
    elif side == 'r':
        if x == -y:
            x -= 1
            side = 'u'
        else:
            y -= 1
    elif side == 'u':
        if x == y:
            y += 1
            side = 'l'
        else:
            x -= 1
    elif side == 'l':
        if x == -y:
            x += 1
            side = 'd'
        else:
            y += 1
    r = 0
    for dy in range(-1, 2):
        r += sum([data[offt+x+dx][offt+y+dy] for dx in range(-1, 2)])
    data[offt+x][offt+y] = r
    if data[offt+x][offt+y] > inp:
        print(data[offt+x][offt+y])
        dump()
        break

print(x, y, side)
