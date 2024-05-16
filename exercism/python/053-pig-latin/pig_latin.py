VOWELS = {'a', 'e', 'i', 'o', 'u'}


def translate_word(word):
    if word[0] in VOWELS or word[0:2] in {'xr', 'yt'}:
        return word + 'ay'

    suffix = ''
    index = 0
    has_consonants = False

    for c in word:
        if has_consonants and c == 'y':
            break
        elif c == 'q' and index + 1 < len(word) and word[index + 1] == 'u':
            suffix += 'qu'
            index += 2
            break
        elif c not in VOWELS:
            suffix += c
            index += 1
            has_consonants = True
        else:
            break

    return word[index:] + suffix + 'ay'


def translate(text):
    return ' '.join(translate_word(word) for word in text.split())
