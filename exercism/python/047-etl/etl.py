def transform(legacy_data):
    result = {}
    for score, letters in legacy_data.items():
        for letter in letters:
            result[letter.lower()] = score
    return result
