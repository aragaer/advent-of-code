#!/usr/bin/env python3

from collections import defaultdict

conn = defaultdict(set)

with open("input12") as inp:
    for line in inp:
        p, ps = [x.strip() for x in line.split('<->')]
        ps = set(int(x.strip()) for x in ps.split(", "))
        conn[int(p)] = ps
        for p2 in ps:
            conn[p2].add(int(p))

d = [-1] * len(conn.keys())
grp = 0
while True:
    for i in conn.keys():
        if d[i] == -1:
            d[i] = 0
            layer = {i}
            break
    else:
        break
    group = set()
    while layer:
        group |= layer
        new_layer = set()
        for p in layer:
            for p2 in conn[p]:
                if d[p2] == -1:
                    d[p2] = i+1
                    new_layer.add(p2)
        layer = new_layer
    print(len(group))
    grp += 1
print(grp)
