#!/usr/bin/env python3

from collections import Counter

count = 0

with open("input4") as inp:
    for line in inp:
        counter = Counter(''.join(sorted(x)) for x in line.split())
        count += all(v==1 for k, v in counter.items())

print(count)
