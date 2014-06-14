import unittest

import my_solution as solution

PERFECT_NUMBERS = {6, 28, 496, 8128}

IMPERFECT_NUMBERS = set(range(1, 100)) - PERFECT_NUMBERS


class TestPerfectNumbers(unittest.TestCase):
    def test_perfect_numbers(self):
        for perfect_number in PERFECT_NUMBERS:
            self.assertTrue(solution.is_perfect(perfect_number),
                            '{} is expected to be a perfect number'.format(
                                perfect_number))

    def test_imperfect_numbers(self):
        for imperfect_number in IMPERFECT_NUMBERS:
            self.assertFalse(solution.is_perfect(imperfect_number),
                             '{} is expected to be an imperfect number'.format(
                                 imperfect_number))


if __name__ == '__main__':
    unittest.main()
