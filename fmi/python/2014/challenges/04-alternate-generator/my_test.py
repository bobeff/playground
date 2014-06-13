import unittest
from itertools import count, islice
from my_solution import alternate


class TestAlternate(unittest.TestCase):
    def test_alternate(self):
        self.assertRaises(StopIteration, lambda: next(alternate()))

        mirror_count = alternate(lambda: count(1, 1), lambda: count(-1, -1))
        self.assertEqual(list(islice(mirror_count, 7)),
                         [1, -1, 2, -2, 3, -3, 4])

if __name__ == '__main__':
    unittest.main()
