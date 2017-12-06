#!/usr/bin/env python3

data = list(map(int, "0	5	10	0	11	14	13	4	11	8	8	7	1	4	12	11".split()))
#data = [0, 2, 7, 0]

past = set()

def reallocate(data):
    m = 0
    p = 0
    for i, v in enumerate(data):
        if v > m:
            m, p = v, i
    data[p] = 0
    for _ in range(m):
        p += 1
        p %= len(data)
        data[p] += 1
    return data

def seen(data):
    d = str(data)
    if d in past:
        return True
    past.add(d)
    return False

count = 0
while not seen(data):
    data = reallocate(data)
    count += 1

print(count)

past = set()
count2 = 0
while not seen(data):
    data = reallocate(data)
    count2 += 1

print(count2)
