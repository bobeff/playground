DOGE_WORDS = {'wow', 'lol', 'so', 'such', 'much', 'very'}


def wow_such_much(start, end):
    return ['such'*(i % 3 == 0) + 'much'*(i % 5 == 0) or str(i)
            for i in range(start, end)]


def count_doge_words(words):
    return sum([words.split().count(word) for word in DOGE_WORDS])
