sequence = set()
for a in range(1, 101):
    for b in range(1, 101):
        sequence.add(a ** b)
print(len(sequence))

