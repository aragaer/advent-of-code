#!/usr/bin/env python3

buf = [0]
pos = 0

if False:
    steps = 3
else:
    steps = 354

for i in range(1, 2017+1):
    pos += steps
    pos %= len(buf)
    buf.insert(pos+1, i)
    pos += 1

print(buf[pos+1])

after_0 = buf[1]
l = len(buf)

for i in range(2017+1, 50000000+1):
    pos += steps
    pos %= l
    l += 1
    if pos == 0:
        after_0 = i
    pos += 1

print(after_0)
