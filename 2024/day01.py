#!/usr/bin/env python3

import sys
from collections import Counter

first, second = [], []
for line in sys.stdin.readlines():
    f, s = map(int, line.strip().split())
    first.append(f)
    second.append(s)

print(sum(abs(f-s) for f, s in zip(sorted(first), sorted(second))))

scnt = Counter(second)
print(sum(k*v*scnt[k] for k, v in Counter(first).items()))
