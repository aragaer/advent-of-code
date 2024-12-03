#!/usr/bin/env python3

import re
import sys

line = sys.stdin.read()
print(line)

res1, res2 = 0, 0
enabled = True
for pat in re.findall(r"mul\((\d+),(\d+)\)|(do)\(\)|(don't)\(\)", line):
    v1, v2, do, dont = pat
    if v1 and v2:
        v = int(v1)*int(v2)
        res1 += v
        if enabled:
            res2 += v
    if do:
        enabled = True
    if dont:
        enabled = False

print(res1, res2, sep='\n')
