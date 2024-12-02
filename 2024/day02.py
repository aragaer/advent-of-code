#!/usr/bin/env python3

import sys

def check_report(report):
    increasing = report[0] < report[1]
    for f, s in zip(report, report[1:]):
        if f == s or abs(f - s) > 3 or (f < s) != increasing:
            return False
    return True

part1, part2 = 0, 0
for line in sys.stdin.readlines():
    report = list(map(int, line.strip().split()))
    if check_report(report):
        part1 += 1
        part2 += 1
    else:
        for i in range(len(report)):
            dampened = report[:i] + report[i+1:]
            if check_report(dampened):
                part2 += 1
                break
print(part1, part2, sep='\n')
