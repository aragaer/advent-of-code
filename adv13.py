#!/usr/bin/env python3

layers = {}

with open("input13") as inp:
    for line in inp:
        l, r = (int(x.strip()) for x in line.split(':'))
        p = r * 2 - 2
        layers[l] = p

def step(position, time):
    cost = 0
    caught = False
    if position in layers and time % layers[position] == 0:
        cost += position * (layers[position] / 2 + 1)
        caught = True
    return cost, caught

def cost_for_delay(delay, cost_only=False):
    last = max(layers.keys())+1

    position = 0
    cost = 0
    while position < last:
        c, caught = step(position, position + delay)
        if caught and not cost_only:
            return 0, True
        cost += c
        position += 1

    return cost, False

print(cost_for_delay(0, True)[0])

delay = 0
while True:
    cost, caught = cost_for_delay(delay)
    if not caught:
        print(delay)
        break
    delay += 1
