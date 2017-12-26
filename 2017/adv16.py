#!/usr/bin/env python3

moves = []
with open("input16") as inp:
    for line in inp:
        moves = line.split(',')

pcount = 16
if len(moves) == 3:
    pcount = 5
programs = [chr(ord('a')+x) for x in range(pcount)]
    
def ex(p, x, y):
    if p == x:
        return y
    if p == y:
        return x
    return p

moves_o = []
for move in moves:
    if move[0] == 's':
        shift = int(move[1:])
        moves_o.append(('s', shift))
    elif move[0] == 'x':
        x, y = move[1:].split('/')
        x, y = int(x), int(y)
        moves_o.append(('x', int(x), int(y)))
    elif move[0] == 'p':
        x, y = move[1:].split('/')
        moves_o.append(('p', x, y))

def combine(moves):
    transp = list(range(pcount))
    for move in moves:
        if move[0] == 's':
            shift = move[1]
            transp = transp[-shift:] + transp[:-shift]
        elif move[0] == 'x':
            x, y = move[1], move[2]
            transp[x], transp[y] = transp[y], transp[x]
    return ('t', transp)

moves_oo = []
collect = []
for move in moves_o:
    if move[0] == 'p':
        moves_oo.append(combine(collect))
        moves_oo.append(move)
        collect = []
    else:
        collect.append(move)
moves_oo.append(combine(collect))

past = {}
def seen(i):
    pt = ''.join(programs)
    if pt in past:
        return True, past[pt]
    past[pt] = i
    return False, None


for i in range(2 if pcount == 5 else 1000000000 % 60):
    if i % 1000 == 0:
        print(i)
    s, p = seen(i)
    if s:
        print("Step", p, "repeated on", i)
        break
    for move in moves_oo:
        if move[0] == 's':
            shift = move[1]
            programs = programs[-shift:] + programs[:-shift]
        elif move[0] == 'x':
            x, y = move[1], move[2]
            programs[x], programs[y] = programs[y], programs[x]
        elif move[0] == 'p':
            x, y = move[1], move[2]
            programs = [ex(p, x, y) for p in programs]
        elif move[0] == 't':
            programs = [programs[t] for t in move[1]]

print(''.join(programs))
