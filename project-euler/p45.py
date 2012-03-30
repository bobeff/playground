#!/usr/bin/env python3.2

def triangolar():
    n = 1
    while True:
        yield n * (n + 1) // 2
        n += 1

def pentagonal():
    n = 1
    while True:
        yield n * (3 * n - 1) // 2
        n += 1

def hexagonal():
    n = 1
    while True:
        yield n * (2 * n - 1)
        n += 1

t285 = p165 = h143 = 40755 

gt = triangolar()
t = next(gt)
while t <= t285: t = next(gt)

gp = pentagonal()
p = next(gp)
while p <= p165: p = next(gp)

gh = hexagonal()
h = next(gh)
while h <= h143: h = next(gh)

while not (t == p == h):
    if t <= min(p, h): t = next(gt)
    elif p <= min(t, h): p = next(gp)
    elif h <= min(t, p): h = next(gh)

print(t)

