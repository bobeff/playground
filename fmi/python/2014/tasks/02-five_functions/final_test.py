import unittest
import types

import my_solution as solution


class TestIsPangram(unittest.TestCase):

    def test_with_pangrams(self):
        self.assertTrue(
            solution.is_pangram(
                'Ах, чудна българска земьо, полюшвай цъфтящи жита!'))

        self.assertTrue(
            solution.is_pangram('За миг бях в чужд, скърцащ плюшен фотьойл.'))

    def test_with_other_sentences(self):
        self.assertFalse(
            solution.is_pangram('Малката пухкава панда яде бамбук.'))

        self.assertFalse(
            solution.is_pangram('Веселите малки зайчета си щъкаха из двора.'))


class TestCharHistogram(unittest.TestCase):

    def test_char_histogram_simple(self):
        self.assertEqual(
            {' ': 3, 'i': 2, 'a': 2, 'e': 2, 's': 2, 'h': 1, 'l': 1, 'm': 1,
             'n': 1, 'x': 1, '!': 1, 'p': 1, 'T': 1},
            solution.char_histogram('This is an example!'))

    def test_char_histogram_with_both_alphabets(self):
        self.assertEqual(
            {' ': 3, 'i': 2, 'e': 2, 'н': 2, 's': 2, 'h': 1, '!': 1, 'l': 1,
             'm': 1, 'с': 1, 'т': 1, '#': 1, 'p': 1, 'x': 1, 'р': 1, 'a': 1,
             'а': 1, 'T': 1, 'е': 1},
            solution.char_histogram("#This is странен example!"))


class TestSortBy(unittest.TestCase):

    def test_sort_by_empty(self):
            self.assertEqual([], solution.sort_by(lambda x, y: x - y, []))

    def test_sort_by_simple_test(self):
        self.assertEqual(
            ['a', 'ab', 'abc'],
            solution.sort_by(lambda x, y: len(x) - len(y), ['abc', 'a', 'ab']))

    def test_sort_by_simple_test(self):
        self.assertEqual(
            [0, 2, 4, 1, 3, 5],
            solution.sort_by(lambda x, y: x % 2 - y % 2, [0, 1, 2, 3, 4, 5]))

    def test_sort_by_one_element(self):
        self.assertEqual([1], solution.sort_by(lambda x, y: x - y, [1]))


class TestGroupByType(unittest.TestCase):

    def test_group_by_type_empty(self):
            self.assertEqual({}, solution.group_by_type({}))

    def test_group_by_type(self):
        self.assertEqual(
            {str: {'b': 1, 'a': 12}, int: {1: 'foo'}},
            solution.group_by_type({'a': 12, 'b': 1, 1: 'foo'}))

    def test_another_group_by_type(self):
        self.assertEqual(
            {str: {'c': 15}, int: {1: 'b'},
             tuple: {(1, 2): 12, ('a', 1): 1}},
            solution.group_by_type({(1, 2): 12, ('a', 1): 1, 1: 'b', 'c': 15}))

    def test_group_by_type_with_frozen_set_key(self):
        test_set = frozenset([1, 2, 3])
        self.assertEqual(
            {frozenset: {test_set: 15}, tuple: {(1, 2): 12, ('a', 1): 1}},
            solution.group_by_type({(1, 2): 12, ('a', 1): 1, test_set: 15}))

    def test_group_by_type_with_functions(self):
        double_lambda = lambda x: 2 * x
        self.assertEqual(
            {
                types.LambdaType: {double_lambda: 'double'},
                str: {'three': 3},
                tuple: {('list', 'of', 'numbers'): [42, 73]}
            },
            solution.group_by_type({
                double_lambda: 'double',
                'three': 3,
                ('list', 'of', 'numbers'): [42, 73]
            })
        )


class TestAnagrams(unittest.TestCase):

    def test_list_of_latin_anagrams(self):
        words = ['army', 'mary', 'ramy', 'astronomer', 'moonstarer',
                 'debit card', 'bad credit', 'bau']
        anagrams = [['army', 'mary', 'ramy'],
                    ['bad credit', 'debit card'],
                    ['astronomer', 'moonstarer'], ['bau']]

        self.assertEqual(
            set(map(frozenset, anagrams)),
            set(map(frozenset, solution.anagrams(words))))

    def test_with_list_of_cyrilic_anagrams(self):
        words = ['кавалер', 'акварел']
        anagrams = [['кавалер', 'акварел']]

        self.assertEqual(
            set(map(frozenset, anagrams)),
            set(map(frozenset, solution.anagrams(words))))

    def test_with_different_cases(self):
        words = ["Dave Barry", "Ray Adverb"]
        anagrams = [["Dave Barry", "Ray Adverb"]]

        self.assertEqual(
            set(map(frozenset, anagrams)),
            set(map(frozenset, solution.anagrams(words))))

    def test_with_different_symbols(self):
        words = ["Tom Marvolo Riddle", "I am Lord Voldemort",
                 "Tom Cruise", "So I'm cuter"]
        anagrams = [["Tom Marvolo Riddle", "I am Lord Voldemort"],
                    ["Tom Cruise", "So I'm cuter"]]

        self.assertEqual(
            set(map(frozenset, anagrams)),
            set(map(frozenset, solution.anagrams(words))))

if __name__ == '__main__':
    unittest.main()
