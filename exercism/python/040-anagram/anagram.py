def find_anagrams(word, candidates):
    word_lower = word.lower()
    word_sorted = sorted(word_lower)
    result = []
    for w in candidates:
        w_lower = w.lower()
        if w_lower != word_lower and sorted(w_lower) == word_sorted:
            result.append(w)
    return result
