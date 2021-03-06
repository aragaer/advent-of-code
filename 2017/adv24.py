#!/usr/bin/env python3

ports = []

with open("input24") as inp:
    for line in inp:
        l, r = line.split('/')
        ports.append((int(l), int(r)))

ms = 0
def build_bridge(n, depth, components):
    result = 0
    length = 0
    bridge = []
    for i, c in enumerate(components):
        nl = 0
        nr = 0
        nb = None
        if c[0] == n:
            nr, nl, nb = build_bridge(c[1], depth+1, components[:i]+components[i+1:])
            nl += 1
            nr += c[0] + c[1]
            nb.append(c)
        elif c[1] == n:
            nr, nl, nb = build_bridge(c[0], depth+1, components[:i]+components[i+1:])
            nl += 1
            nr += c[0] + c[1]
            nb.append(c)
        if nl and nl >= length:
            if nl > length or nr > result:
                result = nr
                bridge = nb
            length = nl
    return result, length, bridge

print(build_bridge(0, 0, ports))
