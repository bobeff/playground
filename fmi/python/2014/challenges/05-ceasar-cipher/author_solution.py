import string

ALPHABET = string.ascii_uppercase


def ceaser_shift(message, offset):
    return ''.join([ALPHABET[(ALPHABET.index(letter) + offset) % len(ALPHABET)]
                    if letter in ALPHABET else letter
                    for letter in message.upper()])


def ceaser_output(offset):
    def decorater(func):
        def decorated(*args, **kwargs):
            return ceaser_shift(func(*args, **kwargs), offset)
        return decorated
    return decorater


def ceaser_input(offset, should_decode):
    def conditional_decode(key, message):
        return ceaser_shift(message, offset) if should_decode(key) else message

    def decorater(func):
        def decorated(*args, **kwargs):
            return func(*[conditional_decode(position, message)
                          for position, message in enumerate(args)],
                        **{key: conditional_decode(key, message)
                           for key, message in kwargs.items()})
        return decorated
    return decorater
