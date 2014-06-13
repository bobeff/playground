import unittest

from my_solution import get_digits, is_happy, is_prime, happy_primes

class TestHappyPrimes(unittest.TestCase):
    HAPPY_NUMBERS = [
         1,  7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91,
        94, 97, 100 ]

    HAPPY_DOUBLES = [129, 192, 262, 301, 319, 367, 391]

    HAPPY_TRIPLES = [1880, 4780, 4870, 7480, 7839]

    PRIME_NUMBERS = [
          2,   3,   5,   7,  11,  13,  17,  19,  23,  29,  31,  37,  41,  43,
         47,  53,  59,  61,  67,  71,  73,  79,  83,  89,  97, 101, 103, 107,
        109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181,
        191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263,
        269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
        353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433,
        439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521,
        523, 541]

    HAPPY_PRIMES = [7, 13, 19, 23, 31, 79, 97, 103, 109, 139]

    def test_get_digits(self):
        self.assertEqual(list(get_digits(1)), [1])
        self.assertEqual(list(get_digits(10)), [0, 1])
        self.assertEqual(list(get_digits(101)), [1, 0, 1])
        self.assertEqual(list(get_digits(31204368)), [8, 6, 3, 4, 0, 2, 1, 3])

    def test_is_happy(self):
        for number in range(1, self.HAPPY_NUMBERS[-1] + 1):
            if number in self.HAPPY_NUMBERS:
                self.assertTrue(is_happy(number))
            else:
                self.assertFalse(is_happy(number))

        for number in self.HAPPY_DOUBLES:
            self.assertTrue(is_happy(number))
            self.assertTrue(is_happy(number + 1))

        for number in self.HAPPY_TRIPLES:
            self.assertTrue(is_happy(number))
            self.assertTrue(is_happy(number + 1))
            self.assertTrue(is_happy(number + 2))

    def test_is_prime(self):
        for number in range(1, self.PRIME_NUMBERS[-1] + 1):
            if number in self.PRIME_NUMBERS:
                self.assertTrue(is_prime(number))
            else:
                self.assertFalse(is_prime(number))

    def test_happy_primes(self):
        self.assertEqual(happy_primes(range(25)), [7, 13, 19, 23])
        self.assertEqual(happy_primes(range(63, 100)), [79, 97])
        self.assertEqual(happy_primes(range(140)), \
            [7, 13, 19, 23, 31, 79, 97, 103, 109, 139])

if __name__ == '__main__':
    unittest.main()
