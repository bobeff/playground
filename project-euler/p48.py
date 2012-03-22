#!/usr/bin/env python3.2

s = 0
for n in range(1, 1001):
    s += n ** n;
print(str(s)[-10:])

