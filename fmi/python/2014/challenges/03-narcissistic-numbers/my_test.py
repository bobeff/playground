import unittest

from my_solution import is_narcissistic


class TestIsNarcissistic(unittest.TestCase):

    def test_is_narcissistic(self):
        self.assertFalse(is_narcissistic('10'))
        self.assertTrue(is_narcissistic('223', 4))
        self.assertTrue(is_narcissistic(
            '115132219018763992565095597973971522401'))
        self.assertTrue(is_narcissistic('C64E7', 16))
        self.assertFalse(is_narcissistic('c64e6', 16))

if __name__ == '__main__':
    unittest.main()
