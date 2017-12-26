#!/usr/bin/env python3

data = list(range(256))
lengths = [ord(x) for x in "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108"]
lengths.extend([17, 31, 73, 47, 23])

skip = 0
position = 0

for _ in range(64):
    for l in lengths:
        _data = data.copy()
        _data.extend(data)
        for i, v in enumerate(reversed(_data[position:position+l])):
            data[(position + i) % len(data)] = v
        position += l + skip
        position %= len(data)
        skip += 1

print(data)

res = []
for p in range(len(data)//16):
    r = 0
    for i in range(16):
        r ^= data[p*16+i]
    res.append(r)

print(res)

print(''.join("%02x" % t for t in res))
