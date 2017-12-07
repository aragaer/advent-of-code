#!/usr/bin/env python3

with open("input7") as inp:
    data = inp.readlines()

towers = {}

for line in data:
    tower = {}
    if '->' in line:
        t, c = line.split('->')
        tower['children'] = c.strip().split(", ")
    else:
        t = line
    name, weight = t.split()
    weight = int(weight.strip()[1:-1])
    tower['weight'] = weight
    towers[name] = tower

for n, t in towers.items():
    for c in t.get('children', []):
        towers[c]['parent'] = n

def total_weight(name):
    return towers[name]['weight'] + sum(total_weight(c) for c in towers[name].get('children', []))

for n, t in towers.items():
    if 'parent' not in t:
        print("Root is", n)
    t['total'] = total_weight(n)

for n, t in towers.items():
    if 'children' in t:
        cw = [towers[c]['total'] for c in t['children']]
        mcw = min(cw)
        Mcw = max(cw)
        if mcw != Mcw:
            print(cw)
            cw.remove(mcw)
            cw.remove(Mcw)
            good = cw[0]
            for c in t['children']:
                if towers[c]['total'] != good:
                    ct = towers[c]
                    print("Total weight of tower", name, "is", ct['total'])
                    print("Other towers on same disk weight", good)
                    print("Own weight is", ct['weight'])
                    print("It should be", ct['weight'] - ct['total'] + good)
