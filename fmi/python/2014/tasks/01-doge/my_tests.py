import unittest

from my_solution import wow_such_much, count_doge_words

class TestDoge(unittest.TestCase):
    def test_wow_such_much(self):
        self.assertEqual(wow_such_much(0, 0), [])
        self.assertEqual(wow_such_much(1, 0), [])
        self.assertEqual(wow_such_much(0, 1), ['suchmuch'])
        self.assertEqual(wow_such_much(-1, 1), ['-1', 'suchmuch'])
        self.assertEqual(wow_such_much(-5, 5),
            ['much', '-4', 'such', '-2', '-1', 'suchmuch', '1', '2', 'such', '4'])

    def test_count_doge_words(self):
        self.assertEqual(count_doge_words(''), 0)
        self.assertEqual(count_doge_words('ala bala'), 0)
        self.assertEqual(count_doge_words('wowlolsosuchmuchvery'), 0)
        self.assertEqual(count_doge_words('lol'), 1)
        self.assertEqual(count_doge_words('wow wow wow'), 3)
        self.assertEqual(count_doge_words('wow lol so such much very'), 6)
        self.assertEqual(count_doge_words('wow ala bala such portokala'), 2)


if __name__ == '__main__':
    unittest.main()
