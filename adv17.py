#!/usr/bin/env python3

buf = [0]
pos = 0

if False:
    steps = 3
else:
    steps = 354

def do_iteration(value):
    global pos
    pos += steps
    pos %= len(buf)
    buf.insert(pos+1, value)
    pos += 1

for i in range(1, 2017+1):
    do_iteration(i)

print(buf[pos-1], buf[pos], buf[pos+1])

after_0 = buf[1]
print(after_0)

l = len(buf)
def do_simpler(value):
    global l, pos, after_0
    pos += steps
    pos %= l
    l += 1
    if pos == 0:
        after_0 = value
    pos += 1

for i in range(2017+1, 50000000+1):
    do_simpler(i)

print(after_0)
