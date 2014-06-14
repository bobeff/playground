from itertools import cycle

def alternate(*generator_buillders):
    for item in cycle(generator() for generator in generator_buillders):
        yield next(item)
