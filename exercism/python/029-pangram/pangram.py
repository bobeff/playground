def is_pangram(sentence):
    return len({c for c in sentence.lower() if c.isalpha()}) == 26
