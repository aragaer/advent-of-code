#!/usr/bin/env python3

with open("input5") as inp:
    instrs = list(map(int, inp.readlines()))

#instrs = [0, 3, 0, 1, -3]

count = 0
pos = 0

while pos >= 0 and pos < len(instrs):
    npos = pos + instrs[pos]
    if instrs[pos] >= 3:
        instrs[pos] -= 1
    else:
        instrs[pos] += 1
    count += 1
    pos = npos

print(count)


