#!/usr/bin/env python3

import re
import yaml

data = []
with open("input25") as inp:
    for line in inp:
        data.append(line)

app = yaml.load("\n".join(data))

initial = re.match("Begin in state (\w).", app[0]).groups(1)[0]
steps = int(re.match("Perform a diagnostic checksum after (\d+) steps.", app[1]).groups(1)[0])
app.pop(0)
app.pop(0)
print(initial, steps)

wr = re.compile(r'Write the value (\d).')
mv = re.compile(r'Move one slot to the (\w+).')
st = re.compile(r'Continue with state (\w).')

tape = [0]
pos = 0
state = initial

for i in range(steps):
    if i % 100000 == 0:
        print(i/steps)
    try:
        val = tape[pos]
        inss = app[ord(state) - ord('A')]["In state {}".format(state)]
        ins = inss[val]['If the current value is {}'.format(val)]
        v = int(wr.search(ins[0]).groups(1)[0])
        d = -1 if mv.search(ins[1]).groups(1)[0] == 'left' else 1
        state = st.search(ins[2]).groups(1)[0]
        tape[pos] = v
        if d == -1 and pos == 0:
            tape.insert(0, 0)
        elif d == 1 and pos == len(tape) - 1:
            tape.append(0)
            pos += 1
        else:
            pos += d
    except:
        print("Error with state {} pos {} val {}".format(state, pos, val))
        raise

print(sum(tape))
