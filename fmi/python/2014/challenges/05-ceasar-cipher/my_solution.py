def cypher(s, shift):
    return ''.join(chr(ord('A') + (ord(c) - ord('A') + shift) % 26)
                   if c.isalpha() else c for c in s.upper())


def ceaser_output(shift):
    def decorator(func):
        def decorated(*args):
            output = func(*args)
            if isinstance(output, str):
                output = cypher(output, shift)
            return output
        return decorated
    return decorator


def ceaser_input(shift, key_func=None):
    def decorator(func):
        def decorated(*args):
            new_args = []
            for i, arg in enumerate(args):
                if isinstance(arg, str) and (not key_func or key_func(i)):
                    new_args.append(cypher(arg, shift))
                else:
                    new_args.append(arg)
            return func(*new_args)
        return decorated
    return decorator
