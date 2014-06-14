import unittest

import my_solution as solution

HAPPY_NUMBERS = {
    1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97,
    100, 103, 109, 129, 130, 133, 139, 167, 176, 188, 190, 192, 193, 203, 208,
    219, 226, 230, 236, 239, 262, 263, 280, 291, 293, 301, 302, 310, 313, 319,
    320, 326, 329, 331, 338, 356, 362, 365, 367, 368, 376, 379, 383, 386, 391,
    392, 397, 404, 409, 440, 446, 464, 469, 478, 487, 490, 496, 536, 556, 563,
    565, 566, 608, 617, 622, 623, 632, 635, 637, 638, 644, 649, 653, 655, 656,
    665, 671, 673, 680, 683, 694, 700, 709, 716, 736, 739, 748, 761, 763, 784,
    790, 793, 802, 806, 818, 820, 833, 836, 847, 860, 863, 874, 881, 888, 899,
    901, 904, 907, 910, 912, 913, 921, 923, 931, 932, 937, 940, 946, 964, 970,
    973, 989, 998, 1000
}

UNHAPPY_NUMBERS = set(range(1, max(HAPPY_NUMBERS))) - HAPPY_NUMBERS


class TestHappyNumbers(unittest.TestCase):
    def test_all_happy_numbers_up_to_a_point(self):
        for happy_number in HAPPY_NUMBERS:
            self.assertTrue(solution.is_happy(happy_number),
                            '{} is expected to be a happy number'.format(
                                happy_number))

    def test_no_unhappy__numbers_up_to_a_point(self):
        for unhappy_number in UNHAPPY_NUMBERS:
            self.assertFalse(solution.is_happy(unhappy_number),
                             '{} is expected to be an unhappy number'.format(
                                 unhappy_number))

    def test_happy_primes_up_to_500(self):
        happy_primes = [7, 13, 19, 23, 31, 79, 97, 103, 109, 139, 167, 193,
                        239, 263, 293, 313, 331, 367, 379, 383, 397, 409, 487]
        self.assertEqual(solution.happy_primes(range(0, 500)), happy_primes)

if __name__ == '__main__':
    unittest.main()
