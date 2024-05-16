from functools import partial


EARTH_YEAR_SECONDS = 31_557_600

PLANET_YEAR_TO_EARTH_YEARS = {
    'mercury':   0.2408467,
    'venus':     0.61519726,
    'earth':     1,
    'mars':      1.8808158,
    'jupiter':  11.862615,
    'saturn':   29.447498,
    'uranus':   84.016846,
    'neptune': 164.79132
}


class SpaceAge:
    def __init__(self, seconds):
        self.seconds = seconds

    def __calc(self, coefficient):
        return round(self.seconds / (coefficient * EARTH_YEAR_SECONDS), 2)

    def __getattr__(self, name):
        return partial(self.__calc, PLANET_YEAR_TO_EARTH_YEARS[name[3:]])
