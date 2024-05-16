from string import ascii_lowercase


def translate(letter):
    return ascii_lowercase[len(ascii_lowercase) -
                           ascii_lowercase.index(letter.lower()) - 1]


def encode(plain_text):
    encoded = ''
    for c in plain_text:
        if c.isalpha():
            encoded += translate(c)
        elif c.isdigit():
            encoded += c

    return ' '.join(encoded[i:i + 5] for i in range(0, len(encoded), 5))


def decode(ciphered_text):
    return ''.join(translate(c) if c.isalpha() else c
                   for c in ciphered_text if not c.isspace())
