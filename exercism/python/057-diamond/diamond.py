def rows(letter):
    start_end_spaces = ord(letter) - ord('A')
    rows = []
    for i in range(0, start_end_spaces + 1):
        rows.append('')
        rows[-1] += ' ' * (start_end_spaces - i)
        rows[-1] += chr(ord('A') + i)
        rows[-1] += ' ' * (i * 2 - 1)
        rows[-1] += chr(ord('A') + i) if i != 0 else ''
        rows[-1] += ' ' * (start_end_spaces - i)
    rows.extend(list(reversed(rows))[1:])
    return rows
