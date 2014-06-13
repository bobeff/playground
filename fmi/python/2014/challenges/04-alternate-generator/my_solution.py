from itertools import cycle


def alternate(*args):
    iterators = (arg() for arg in args)
    for iter in cycle(iterators):
        yield next(iter)
