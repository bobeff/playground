def is_isogram(string):
    chars = set()
    for c in string:
        c = c.lower()
        if c in chars:
            return False
        if c.isalpha():
            chars.add(c)
    return True
