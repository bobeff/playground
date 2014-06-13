import unittest

from my_solution import wow_such_much, count_doge_words


class TestDoge(unittest.TestCase):
    def test_suchmuch_to_16(self):
        self.assertEqual(
            wow_such_much(1, 16),
            ['1', '2', 'such', '4', 'much', 'such', '7', '8', 'such', 'much',
             '11', 'such', '13', '14', 'suchmuch'])

    def test_with_simple_sentence(self):
        self.assertEqual(3, count_doge_words("wow much hard such difficult"))


if __name__ == '__main__':
    unittest.main()
