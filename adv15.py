#!/usr/bin/env python3

factorA = 16807
factorB = 48271
base = 2147483647
A = 699
B = 124

if False:
    A = 65
    B = 8921

def step(start, factor, mul):
    result = start * factor % base
    while result % mul:
        result = result * factor % base
    return result

result = 0
for _ in range(5000000):
    A, B = step(A, factorA, 4), step(B, factorB, 8)
    if A % 65536 == B % 65536:
        result += 1

print(result)
