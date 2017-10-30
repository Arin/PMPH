#!/usr/bin/python
import random

data = []
i = 0
while i < 1000000:
    data.append(random.randrange(-100, 101))
    i += 1
print data
