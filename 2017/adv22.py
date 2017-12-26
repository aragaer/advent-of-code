#!/usr/bin/env python3

lines = []
with open("input22") as inp:
    for line in inp:
        lines.append(['i' if c == '#' else 'c' for c in line.strip()])

center = (len(lines) // 2, len(lines[0]) // 2)
position = (0, 0)

direction = 'u'

TR={'u': 'r', 'r': 'd', 'd': 'l', 'l': 'u'}
TL={'u': 'l', 'l': 'd', 'd': 'r', 'r': 'u'}
TB={'u': 'd', 'd': 'u', 'l': 'r', 'r': 'l'}
NT={'u': 'u', 'd': 'd', 'l': 'l', 'r': 'r'}
NS={'c': 'w', 'w': 'i', 'i': 'f', 'f': 'c'}
RT={'c': TL, 'w': NT, 'i': TR, 'f': TB}

mem = {}

for i, l in enumerate(lines):
    for j, x in enumerate(l):
        mem[(j-center[0], i-center[1])] = x


infections = 0

for _ in range(10000000):
    status = mem.get(position, 'c')
    direction = RT[status][direction]
    mem[position] = NS[status]
    infections += status == 'w'
    if direction == 'u':
        position = position[0], position[1]-1
    elif direction == 'd':
        position = position[0], position[1]+1
    elif direction == 'r':
        position = position[0]+1, position[1]
    elif direction == 'l':
        position = position[0]-1, position[1]

print(infections)
