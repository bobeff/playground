import string
import unittest

import my_solution as solution


class TestCaesarCipher(unittest.TestCase):

    def test_ceaser_output(self):
        decorator = solution.ceaser_output(13)
        cross_the_river = lambda: "ALEA IACTA EST"
        decorated = decorator(cross_the_river)
        self.assertEqual(decorated(), "NYRN VNPGN RFG")

    def test_output_with_overflowing(self):
        decorator = solution.ceaser_output(23)
        cross_the_river = lambda: string.ascii_uppercase
        decorated = decorator(cross_the_river)
        self.assertEqual(decorated(), "XYZABCDEFGHIJKLMNOPQRSTUVW")

    def test_ceaser_input(self):
        decorator = solution.ceaser_input(-13, lambda key: key > 0)

        def make_a_speech(name, *args):
            return '{} says:\n{}'.format(name, ' '.join(args))

        decorated = decorator(make_a_speech)
        real_input = decorated('Reg', 'JUNG', 'UNIR', 'GUR', 'EBZNAF',
                               'RIRE', 'QBAR', 'SBE', 'HF?', '...')
        expected_input = """Reg says:
WHAT HAVE THE ROMANS EVER DONE FOR US? ..."""
        self.assertEqual(real_input, expected_input)


    def test_the_quick_brown_fox(self):
        message = 'the quick brown fox jumps over the lazy dog'.upper()
        encoded = 'QEB NRFZH YOLTK CLU GRJMP LSBO QEB IXWV ALD'

        decorator = solution.ceaser_output(3)
        self.assertEqual(decorator(lambda: encoded)(), message)

if __name__ == '__main__':
    unittest.main()
