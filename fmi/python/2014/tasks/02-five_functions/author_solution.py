from collections import Counter, defaultdict
from functools import cmp_to_key
import string

CYRILIC = 'абвгдежзийклмнопрстуфхцчшщъьюя'
LATIN = string.ascii_lowercase
ALPHABETS = CYRILIC + LATIN


def is_pangram(sentence):
    return all(map(lambda letter: letter in sentence.lower(), CYRILIC))


def char_histogram(text):
    return Counter(text)


def sort_by(func, arguments):
    return sorted(arguments, key=cmp_to_key(func))


def group_by_type(dictionary):
    result = defaultdict(dict)
    for key, value in dictionary.items():
        result[type(key)][key] = value
    return result


def anagrams(words):
    result = set()
    for word in words:
        group = {w for w in words if
                 Counter(''.join([c for c in w.lower() if c in ALPHABETS])) ==
                 Counter(''.join([c for c in word.lower() if c in ALPHABETS]))}
        result.add(tuple(group))
    return [list(element) for element in result]
