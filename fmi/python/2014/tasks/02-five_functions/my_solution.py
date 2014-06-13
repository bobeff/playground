from collections import defaultdict
from functools import cmp_to_key

BG_LETTERS = "абвгдежзийклмнопрстуфхцчшщъьюя"


def is_pangram(sentence):
    return len({letter for letter in sentence.lower()
               if letter in BG_LETTERS}) == 30


def char_histogram(text):
    histogram = defaultdict(int)
    for character in text:
        histogram[character] += 1
    return histogram


def sort_by(func, arguments):
    return sorted(arguments, key=cmp_to_key(func))


def group_by_type(dictionary):
    type_dict = defaultdict(dict)
    for key, value in dictionary.items():
        type_dict[type(key)][key] = value
    return type_dict


def anagrams(words):
    sorted_words = [''.join(sorted(word)) for word in
                    (filter(lambda c: c.isalpha(), w.lower()) for w in words)]
    result = defaultdict(list)
    for i in range(len(words)):
        result[sorted_words[i]].append(words[i])
    return list(result.values())
