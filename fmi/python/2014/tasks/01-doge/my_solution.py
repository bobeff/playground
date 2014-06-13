def wow_such_much(start, end):
    def transform_item(item):
        if item % 3 == 0 and item % 5 == 0:
            return 'suchmuch'
        elif item % 3 == 0:
            return 'such'
        elif item % 5 == 0:
            return 'much'
        else:
            return str(item)

    return [transform_item(item) for item in range(start, end)]

PARASITE_WORDS = ['wow', 'lol', 'so', 'such', 'much', 'very']

def count_doge_words(sentance):
    return sum(1 for word in sentance.split() if word in PARASITE_WORDS)
