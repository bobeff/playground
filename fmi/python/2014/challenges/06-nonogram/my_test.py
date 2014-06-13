import unittest

from my_solution import validate_nonogram


class TestValidatenonogram(unittest.TestCase):

    def test_statement_example(self):
        nonogram = [[' ', ' ', ' ', 'X', ' '],
                    [' ', 'X', 'X', 'X', 'X'],
                    ['X', 'X', 'X', ' ', 'X'],
                    ['X', 'X', 'X', ' ', ' '],
                    ['X', ' ', ' ', ' ', ' ']]

        keys = {'rows': [[1], [4], [3, 1], [3], [1]],
                'columns': [[3], [3], [3], [2], [2]]}

        self.assertTrue(validate_nonogram(nonogram, keys))

    def test_failure(self):
        nonogram = [[' ', ' ', ' ', 'X', ' '],
                    [' ', 'X', 'X', 'X', 'X'],
                    ['X', 'X', 'X', ' ', 'X'],
                    ['X', 'X', 'X', ' ', ' '],
                    [' ', ' ', ' ', ' ', ' ']]

        keys = {'rows': [[1], [4], [3, 1], [3], [1]],
                'columns': [[3], [3], [3], [2], [2]]}

        self.assertFalse(validate_nonogram(nonogram, keys))

if __name__ == '__main__':
    unittest.main()
