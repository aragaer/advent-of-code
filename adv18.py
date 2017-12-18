#!/usr/bin/env python3

prg = []

with open("input18") as inp:
    for line in inp:
        prg.append(line.strip())

letters = list(chr(ord('a') + x) for x in range(26))
queue0 = []
queue1 = []

class Program:

    def __init__(self, ID, queue, out):
        self.played = None
        self.pos = 0
        self.regs = dict((l, 0) for l in letters)
        self.queue = queue
        self.out = out
        self.regs['p'] = ID
        self.count = 0

    def arg2val(self, arg):
        if arg in letters:
            return self.regs[arg]
        return int(arg)

    def run(self):
        while True:
            cmd, args = prg[self.pos].split(" ", 1)
            if cmd == 'jgz':
                x, y = args.split()
                y = self.arg2val(y)
                if self.arg2val(x) > 0:
                    self.pos += y
                else:
                    self.pos += 1
            else:
                if cmd == 'snd':
                    self.out.append(self.arg2val(args))
                    self.count += 1
                elif cmd == 'rcv':
                    if not self.queue:
                        break
                    self.regs[args] = self.queue.pop(0)
                elif cmd == 'set':
                    x, y = args.split()
                    self.regs[x] = self.arg2val(y)
                elif cmd == 'add':
                    x, y = args.split()
                    self.regs[x] += self.arg2val(y)
                elif cmd == 'mul':
                    x, y = args.split()
                    self.regs[x] *= self.arg2val(y)
                elif cmd == 'mod':
                    x, y = args.split()
                    self.regs[x] %= self.arg2val(y)
                else:
                    raise Exception("Huh? {} {}".format(cmd, args))
                self.pos += 1


p0 = Program(0, queue0, queue1)
p1 = Program(1, queue1, queue0)
p0.run()
p1.run()
while queue0 or queue1:
    p0.run()
    p1.run()
print(p1.count)
    
