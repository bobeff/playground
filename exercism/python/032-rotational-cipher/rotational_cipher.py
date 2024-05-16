from string import ascii_lowercase, ascii_uppercase


def rotate(text, key):
    if key < 0 or key > 26:
        raise ValueError('Invalid rotation key.')
    rotated_ascii_lowercase = ascii_lowercase[key:] + ascii_lowercase[:key]
    rotated_ascii_uppercase = ascii_uppercase[key:] + ascii_uppercase[:key]
    mapping = str.maketrans(ascii_lowercase + ascii_uppercase,
                            rotated_ascii_lowercase + rotated_ascii_uppercase)
    return text.translate(mapping)
