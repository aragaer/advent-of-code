#!/usr/bin/env python3
from collections import defaultdict
import re

regs = defaultdict(lambda: 0)

ins = re.compile(r'([a-z]+) (inc|dec) (-?\d+) if ([a-z]+) ([<>=!]=?) (-?\d+)')

m = 0
with open("input8") as inp:
    for line in inp:
        match = ins.match(line)
        if match is None:
            print(line)
            print("BAD")
            break
        reg, op, val, creg, cop, cval = match.groups()
        creg_val = regs[creg]
        if eval("{} {} {}".format(creg_val, cop, cval)):
            if op == 'inc':
                regs[reg] += int(val)
            else:
                regs[reg] -= int(val)
        if regs[reg] > m:
            m = regs[reg]
print(max(regs.values()))
print(m)
