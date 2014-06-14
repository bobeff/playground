import unittest
import my_solution as solution
from itertools import count, repeat


class TestAlternatingGenerator(unittest.TestCase):
    def test_mirror_count(self):
        mirror_count = solution.alternate(lambda: count(1, 1),
                                          lambda: count(-1, -1))

        self.assertEqual([next(mirror_count) for i in range(1000)],
                         [x for n in range(1, 501) for x in (n, -n)])

    def test_with_finishing_iterable(self):
        alternator = solution.alternate(lambda: repeat("He's coming in .."),
                                        lambda: iter([3, 2, 1]))

        self.assertEqual(list(alternator), ["He's coming in ..", 3,
                                            "He's coming in ..", 2,
                                            "He's coming in ..", 1,
                                            "He's coming in .."])

    def test_with_seven_iterables(self):
        alternator = solution.alternate(lambda: count(0, 7),
                                        lambda: count(1, 7),
                                        lambda: count(2, 7),
                                        lambda: count(3, 7),
                                        lambda: count(4, 7),
                                        lambda: count(5, 7),
                                        lambda: count(6, 7))
        self.assertEqual([next(alternator) for i in range(49)],
                         list(range(49)))


if __name__ == '__main__':
    unittest.main()
