import unittest

import my_solution as solution


class TestSuchMuchDoge(unittest.TestCase):
    def test_suchmuch_to_0(self):
        self.assertEqual(solution.wow_such_much(1, 0), [])

    def test_suchmuch_to_1(self):
        self.assertEqual(solution.wow_such_much(1, 1), [])

    def test_suchmuch_to_2(self):
        self.assertEqual(solution.wow_such_much(1, 2), ['1'])

    def test_suchmuch_to_16(self):
        self.assertEqual(
            solution.wow_such_much(1, 16),
            ['1', '2', 'such', '4', 'much', 'such', '7', '8', 'such', 'much',
             '11', 'such', '13', '14', 'suchmuch'])

    def test_suchmuch_from_16_to_16(self):
        self.assertEqual(solution.wow_such_much(16, 16), [])

    def test_suchmuch_to_100(self):
        self.assertEqual(
            solution.wow_such_much(1, 100),
            ['1', '2', 'such', '4', 'much', 'such', '7', '8', 'such', 'much',
             '11', 'such', '13', '14', 'suchmuch', '16', '17', 'such', '19',
             'much', 'such', '22', '23', 'such', 'much', '26', 'such', '28',
             '29', 'suchmuch', '31', '32', 'such', '34', 'much', 'such', '37',
             '38', 'such', 'much', '41', 'such', '43', '44', 'suchmuch', '46',
             '47', 'such', '49', 'much', 'such', '52', '53', 'such', 'much',
             '56', 'such', '58', '59', 'suchmuch', '61', '62', 'such', '64',
             'much', 'such', '67', '68', 'such', 'much', '71', 'such', '73',
             '74', 'suchmuch', '76', '77', 'such', '79', 'much', 'such', '82',
             '83', 'such', 'much', '86', 'such', '88', '89', 'suchmuch', '91',
             '92', 'such', '94', 'much', 'such', '97', '98', 'such'])

    def test_negative_suchmuch(self):
        self.assertEqual(solution.wow_such_much(16, 1), [])


class TestDogeSay(unittest.TestCase):
    def test_with_simple_sentence(self):
        self.assertEqual(3, solution.count_doge_words("wow much hard such difficult"))

    def test_sentence_without_slangs(self):
        self.assertEqual(0, solution.count_doge_words("The quick brown fox jumps over"))

    def test_sentence_with_parasite_word_as_part_of_normal_one(self):
        self.assertEqual(2, solution.count_doge_words("wow soon hard such difficult"))

    def test_with_empty_sentence(self):
        self.assertEqual(0, solution.count_doge_words(""))

    def test_with_parasite_sentence(self):
        self.assertEqual(6, solution.count_doge_words('wow lol so such much very'))

    def test_with_glued_parasite_words(self):
        self.assertEqual(0, solution.count_doge_words('wowlolsosuchmuchvery'))

    def test_with_repeating_parasite_words(self):
        self.assertEqual(5, solution.count_doge_words('wow hard wow much such difficult much'))

    def test_with_more_space_between_parasites(self):
        self.assertEqual(6, solution.count_doge_words('wow  lol    so  such   much      very'))


if __name__ == '__main__':
    unittest.main()
