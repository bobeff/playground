import unittest

from my_solution import is_perfect

class TestIsPerfect(unittest.TestCase):
    def test_perfects(self):
        self.assertTrue(is_perfect(6))
        self.assertTrue(is_perfect(28))
        self.assertTrue(is_perfect(496))
        self.assertTrue(is_perfect(8128))
        self.assertTrue(is_perfect(33550336))

    def test_not_perfect(self):
        self.assertFalse(is_perfect(0))
        self.assertFalse(is_perfect(1))
        self.assertFalse(is_perfect(10))
        self.assertFalse(is_perfect(3423))

if __name__ == '__main__':
    unittest.main()
