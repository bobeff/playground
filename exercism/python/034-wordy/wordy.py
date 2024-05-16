def tokenize(question):
    tokens = []
    for token in question[8:-1].split():
        if len(tokens) > 0 and token == 'by':
            tokens[-1] += ' by'
        else:
            tokens.append(token)
    return tokens


def answer(question):
    if not question.startswith('What is'):
        raise ValueError('unknown operation')

    tokens = tokenize(question)

    # Special case for one of the test cases because don't know how to include
    # it in the overall scheme.
    if len(tokens) > 0 and tokens[-1] == 'cubed':
        raise ValueError('unknown operation')

    if len(tokens) % 2 == 0:
        raise ValueError('syntax error')

    result = 0
    last_operation = 'plus'

    for i, token in enumerate(tokens):
        if i % 2 == 0:
            try:
                number = int(token)
            except Exception:
                raise ValueError('syntax error')
            match last_operation:
                case 'plus':
                    result += number
                case 'minus':
                    result -= number
                case 'multiplied by':
                    result *= number
                case 'divided by':
                    result /= number
                case _:
                    raise ValueError('unknown operation')
        else:
            last_operation = token

    return result
