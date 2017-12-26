#!/usr/bin/env python3

key = "uugsqrei"

def knot_hash(lengths):
    data = list(range(256))
    lengths.extend([17, 31, 73, 47, 23])
    skip = 0
    position = 0

    for _ in range(64):
        for l in lengths:
            _data = data[:]
            _data.extend(data)
            for i, v in enumerate(reversed(_data[position:position+l])):
                data[(position + i) % len(data)] = v
            position += l + skip
            position %= len(data)
            skip += 1
    res = []
    for p in range(len(data)//16):
        r = 0
        for i in range(16):
            r ^= data[p*16+i]
        res.append(r)
    return res

#key = "flqrgnkx"

result = 0
mem = ""
for i in range(128):
    row = "{}-{}".format(key, i)
    data = [ord(x) for x in row]
    hash = knot_hash(data)
    bin = ''.join(["{0:8b}".format(x) for x in hash])
    mem += bin
    result+=sum(x == '1' for x in bin)

print(result)

def neighs(pos):
    result = []
    x = pos % 128
    y = pos // 128
    if x:
        result.append(pos-1)
    if x < 127:
        result.append(pos+1)
    if y:
        result.append(pos-128)
    if y < 127:
        result.append(pos+128)
    return result

mem = list(mem)

count = 0
for p, v in enumerate(mem):
    if v != '1':
        continue
    count += 1
    queue = [p]
    while queue:
        nq = []
        for p in queue:
            if mem[p] != '1':
                continue
            mem[p] = '2'
            nq.extend(neighs(p))
        queue = nq
    
print(count)
