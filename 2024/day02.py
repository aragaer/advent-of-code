#!/usr/bin/env python3

import sys

def check_report(report):
    asc = report[0] < report[1]
    return all(f != s and abs(f - s) < 4 and (f < s) == asc
               for f, s in zip(report, report[1:]))

part1, part2 = 0, 0
for line in sys.stdin.readlines():
    report = list(map(int, line.strip().split()))
    if check_report(report):
        part1 += 1
    elif any(check_report(report[:i] + report[i+1:]) for i in range(len(report))):
        part2 += 1

print(part1, part1+part2, sep='\n')
