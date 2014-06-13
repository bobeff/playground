import unittest

from my_solution import \
    is_pangram, char_histogram, sort_by, group_by_type, anagrams


class TestFiveFunctions(unittest.TestCase):

    def test_is_pangram(self):
        self.assertFalse(
            is_pangram('Малката пухкава панда яде бамбук.'))

        self.assertTrue(
            is_pangram(
                'Ах, чудна българска земьо, полюшвай цъфтящи жита!'))

    def test_char_histogram(self):
        self.assertEqual(
            {' ': 3, 'i': 2, 'a': 2, 'e': 2, 's': 2, 'h': 1, 'l': 1, 'm': 1,
             'n': 1, 'x': 1, '!': 1, 'p': 1, 'T': 1},
            char_histogram('This is an example!'))

    def test_sort_by(self):
        self.assertEqual(
            ['a', 'ab', 'abc'],
            sort_by(lambda x, y: len(x) - len(y), ['abc', 'a', 'ab']))

    def test_group_by_type(self):
        self.assertEqual(
            {str: {'b': 1, 'a': 12}, int: {1: 'foo'}},
            group_by_type({'a': 12, 'b': 1, 1: 'foo'}))

        self.assertEqual(
            {str: {'c': 15}, int: {1: 'b'},
             tuple: {(1, 2): 12, ('a', 1): 1}},
            group_by_type({(1, 2): 12, ('a', 1): 1, 1: 'b', 'c': 15}))

    def test_anagrams(self):
        words = ['army', 'mary', 'ramy', 'astronomer', 'moonstarer',
                 'debit card', 'bad credit', 'bau']
        anagram_words = [['army', 'mary', 'ramy'],
                         ['bad credit', 'debit card'],
                         ['astronomer', 'moonstarer'], ['bau']]
        self.assertEqual(
            set(map(frozenset, anagram_words)),
            set(map(frozenset, anagrams(words))))


if __name__ == '__main__':
    unittest.main()
