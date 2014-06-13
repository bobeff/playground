import unittest
from my_solution import *


class TestCeaserCipher(unittest.TestCase):
    def test_cypher(self):
        self.assertEqual(cypher("", 5), "")
        self.assertEqual(cypher("a", 1), "B")
        self.assertEqual(cypher("   ", 3), "   ")
        self.assertEqual(cypher("ala bala", 0), "ALA BALA")
        self.assertEqual(cypher("ala bala", 2), "CNC DCNC")
        self.assertEqual(cypher("ala bala", -1), "ZKZ AZKZ")
        self.assertEqual(cypher("zzz", 2), "BBB")

    def test_ceaser_output(self):

        @ceaser_output(13)
        def cross_the_river():
            return "ALEA IACTA EST"

        self.assertEqual(cross_the_river(), 'NYRN VNPGN RFG')

    def test_ceaser_input(self):

        @ceaser_input(-13, lambda key: key > 0)
        def make_a_speech(name, *args):
            return '{} says:\n{}'.format(name, ' '.join(args))

        self.assertEqual(make_a_speech('Reg', 'JUNG', 'UNIR', 'GUR', 'EBZNAF',
                                       'RIRE', 'QBAR', 'SBE', 'HF?', '...'),
                         'Reg says:\n'
                         'WHAT HAVE THE ROMANS EVER DONE FOR US? ...')

    def test_ceaser_input_no_func(self):

        @ceaser_input(-13)
        def make_a_speech(name, *args):
            return '{} says:\n{}'.format(name, ' '.join(args))

        self.assertEqual(make_a_speech('Reg', 'JUNG', 'UNIR', 'GUR', 'EBZNAF',
                                       'RIRE', 'QBAR', 'SBE', 'HF?', '...'),
                         'ERT says:\n'
                         'WHAT HAVE THE ROMANS EVER DONE FOR US? ...')

if __name__ == '__main__':
    unittest.main()
