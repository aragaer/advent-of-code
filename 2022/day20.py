#!/usr/bin/env python3

import sys

def read_all():
    while True:
        try:
            s = input()
            yield int(s)
        except EOFError:
            break

class Threaded:
    def __init__(self, lst):
        self.data = lst
        self.forward = list(range(1,len(lst)+1))
        self.forward.append(1)
        self.backward = list(range(-1,len(lst)))
        self.backward[0] = self.backward[1] = len(lst)

    def cut(self,idx):
        n = self.forward[idx]
        p = self.backward[idx]
        self.forward[p] = n
        self.backward[n] = p
        if self.forward[0] == idx:
            self.forward[0] = n
        if self.backward[0] == idx:
            self.backward[0] = p

    def insert(self,idx,pos):
        n = self.forward[pos]
        p = pos
        self.forward[idx] = n
        self.forward[p] = idx
        self.backward[idx] = p
        self.backward[n] = idx
        if self.backward[0] == p:
            self.backward[0] = idx

    def shift(self,idx,by):
        if by > 0:
            for _ in range(by):
                idx = self.forward[idx]
        else:
            for _ in range(1-by):
                idx = self.backward[idx]
        return idx

    def move(self,pos,by):
        if by == 0:
            return
        np = self.shift(pos,by)
        #print("move from", pos, "by", by, "to", self.forward[np])
        self.cut(pos)
        self.insert(pos,np)

    def iter(self):
        idx = self.forward[0]
        stop = idx
        while True:
            yield self.data[idx-1]
            idx = self.forward[idx]
            if idx == stop:
                break

    def riter(self):
        idx = self.backward[0]
        stop = idx
        while True:
            yield self.data[idx-1]
            idx = self.backward[idx]
            if idx == stop:
                break

    def mix(self,times=1):
        l = len(self.data)
        for _ in range(times):
            for i,x in enumerate(self.data):
                ax = (x + l // 2) % (l-1) - l // 2
                self.move(i+1,ax)
                if False:
                    print(x, ax, list(self.iter()),
                          list(reversed(list(self.riter())))==list(self.iter()),
                          sep="\t")

    def result(self):
        i0 = self.data.index(0)+1
        i1 = self.shift(i0,1000)
        i2 = self.shift(i1,1000)
        i3 = self.shift(i2,1000)
        return self.data[i1-1]+self.data[i2-1]+self.data[i3-1]

def main():
    data = list(read_all())
    threaded = Threaded(data)
    threaded.mix()
    print(threaded.result())
    data2 = [i*811589153 for i in data]
    threaded2 = Threaded(data2)
    threaded2.mix(10)
    print(threaded2.result())

if __name__ == '__main__':
    main()
