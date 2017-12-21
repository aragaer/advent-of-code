#!/usr/bin/env python3

from itertools import chain

rules = {}

with open("input21") as inp:
    for line in inp:
        k, v = line.split('=>')
        rules[k.strip()] = v.strip()

data = [[0, 1, 0], [0, 0, 1], [1, 1, 1]]

if len(rules) == 2:
    iter = 2
else:
    iter = 5


def cut(x, y, sz):
    return list(l[y*sz:y*sz+sz] for l in data[x*sz:x*sz+sz])

def to_text(piece):
    return '/'.join(''.join('#' if x else '.' for x in l) for l in piece)

def from_text(t):
    result = []
    for l in t.split('/'):
        result.append([1 if c == '#' else 0 for c in l])
    return result

def flip(piece):
    return [list(reversed(l)) for l in piece]

def rotate(piece):
    result = [[] for _ in piece]
    for i, l in enumerate(piece):
        for j, x in enumerate(reversed(l)):
            result[j].append(x)
    return result

def replacement(x, y, sz):
    piece = cut(x, y, sz)
    for p in (piece, flip(piece)):
        for _ in range(4):
            if to_text(p) in rules:
                return from_text(rules[to_text(p)])
            else:
                p = rotate(p)
    raise Exception("No rule for {}".format(to_text(piece)))


iter = 18

for _ in range(iter):
    if len(data) %2 == 0:
        sz = 2
    elif len(data) %3 == 0:
        sz = 3
    else:
        raise Exception("Not multiple of 2 or 3")
    result = []
    for x in range(len(data)//sz):
        line = []
        for y in range(len(data)//sz):
            line.append(replacement(x, y, sz))
        wline = []
        for _ in range(sz+1):
            wline.append([])
        for i in range(sz+1):
            for m in line:
                wline[i].extend(m[i])
        result.extend(wline)
    data = result


print(sum(chain(*data)))
