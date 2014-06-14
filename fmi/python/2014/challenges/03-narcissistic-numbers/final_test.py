import unittest

import my_solution as solution


class TestNarcissisticNumbers(unittest.TestCase):
    def test_default_base_is_ten(self):
        self.assertTrue(solution.is_narcissistic('153'))

    def test_with_numbers_in_base_ten(self):
        self.assertTrue(solution.is_narcissistic('32164049651'))
        self.assertFalse(solution.is_narcissistic('6325629'))

    def test_with_numbers_in_lower_bases(self):
        self.assertTrue(solution.is_narcissistic('1', 2))
        self.assertFalse(solution.is_narcissistic('11', 5))
        self.assertTrue(solution.is_narcissistic('13', 7))

    def test_with_with_numbers_in_higher_bases_with_digits_less_than_9(self):
        self.assertFalse(solution.is_narcissistic('765', 15))
        self.assertTrue(solution.is_narcissistic('192', 23))
        self.assertFalse(solution.is_narcissistic('9632', 36))

    def test_with_numbers_in_higher_bases_with_digits_greater_than_9(self):
        self.assertTrue(solution.is_narcissistic('A', 11))
        self.assertTrue(solution.is_narcissistic('C60E7', 16))

if __name__ == '__main__':
    unittest.main()
