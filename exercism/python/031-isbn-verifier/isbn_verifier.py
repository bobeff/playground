def is_valid(isbn):
    isbn = ''.join(c for c in isbn if c != '-')
    if len(isbn) != 10:
        return False
    sum = 0
    for pos, c in enumerate(isbn, 1):
        if c.isdigit():
            sum += (11 - pos) * int(c)
        elif pos == 10 and c == 'X':
            sum += 10
        else:
            return False
    return sum % 11 == 0
