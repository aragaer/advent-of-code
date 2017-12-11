#!/usr/bin/env python3

x = 0
y = 0
with open("input11") as inp:
    steps = inp.readline()

#steps = "ne,ne,ne"
#steps = "ne,ne,sw,sw"
#steps = "ne,ne,s,s"
#steps = "se,sw,se,sw,sw"

m = 0

def away(x, y):
    x = abs(x)
    y = abs(y)

    result = 0

    while x and y:
        if x == y:
            x -= 1
            y -= 1
        elif x > y:
            x -= 2
        elif x < y:
            y -= 2
        result += 1
    return result

for step in steps.split(','):
    if step == 'ne':
        x += 1
        y += 1
    elif step == 'sw':
        x -= 1
        y -= 1
    elif step == 's':
        y -= 2
    elif step == 'se':
        x += 1
        y -= 1
    elif step == 'n':
        y += 2
    elif step == 'nw':
        y += 1
        x -= 1
    else:
        raise "Huh? " + step
    d = away(x, y)
    if d > m:
        m = d


print(away(x,y))

print(m)
