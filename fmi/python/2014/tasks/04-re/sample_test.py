import unittest, textwrap, re

from my_solution import PrivacyFilter, Validations


class PrivacyFilterBiggerTest(unittest.TestCase):
    def __init__(self, *args, **kwargs):
        super(PrivacyFilterBiggerTest, self).__init__(*args, **kwargs)

        self.TEXT = textwrap.dedent("""\
            Hi my name is Ivan and my phone is 0 (888) 87-70-50. In interantional format it is writen as
            +359  888  87  70  50 or 00359 (888) 877050. My email is bobef@gmail.com and alternative
            ivan.bobef@abv.bg. I have a problem with order from Amazon. The e-mail of their support is
            support@amazon.co.uk. My social sequrity number is 0-1-2-3-4-5 and EGN 8510135927.""")

        self.FILTERED_TEXT = textwrap.dedent("""\
            Hi my name is Ivan and my phone is [PHONE]. In interantional format it is writen as
            [PHONE] or [PHONE]. My email is [EMAIL] and alternative
            [EMAIL]. I have a problem with order from Amazon. The e-mail of their support is
            [EMAIL]. My social sequrity number is 0-1-2-3-4-5 and EGN 8510135927.""")

        self.FILTERED_TEXT_PRESERVE_COUNTRY_CODE_AND_HOSTNAME = textwrap.dedent("""\
            Hi my name is Ivan and my phone is [PHONE]. In interantional format it is writen as
            +359 [FILTERED] or 00359 [FILTERED]. My email is [FILTERED]@gmail.com and alternative
            [FILTERED]@abv.bg. I have a problem with order from Amazon. The e-mail of their support is
            [FILTERED]@amazon.co.uk. My social sequrity number is 0-1-2-3-4-5 and EGN 8510135927.""")

        self.FILTERED_TEXT_PARTIALLY_PRESERVE_USERNAME = textwrap.dedent("""\
            Hi my name is Ivan and my phone is [PHONE]. In interantional format it is writen as
            [PHONE] or [PHONE]. My email is [FILTERED]@gmail.com and alternative
            iva[FILTERED]@abv.bg. I have a problem with order from Amazon. The e-mail of their support is
            sup[FILTERED]@amazon.co.uk. My social sequrity number is 0-1-2-3-4-5 and EGN 8510135927.""")

        self.filter = PrivacyFilter(self.TEXT)

    def test_filter(self):
        self.filter.preserve_phone_country_code = False
        self.filter.preserve_email_hostname = False
        self.filter.partially_preserve_email_username = False
        self.assertEqual(self.FILTERED_TEXT, self.filter.filtered())

    def test_filter_preserve_country_code_and_hostname(self):
        self.filter.preserve_phone_country_code = True
        self.filter.preserve_email_hostname = True
        self.filter.partially_preserve_email_username = False
        self.assertEqual(self.FILTERED_TEXT_PRESERVE_COUNTRY_CODE_AND_HOSTNAME, self.filter.filtered())

    def test_filter_partially_preserve_username(self):
        self.filter.preserve_phone_country_code = False
        self.filter.preserve_email_hostname = False
        self.filter.partially_preserve_email_username = True
        self.assertEqual(self.FILTERED_TEXT_PARTIALLY_PRESERVE_USERNAME, self.filter.filtered())


class PrivacyFilterTest(unittest.TestCase):
    def test_obfuscates_simple_emails(self):
        self.assertEqual('Contact: [EMAIL]', PrivacyFilter('Contact: someone@example.com').filtered())

    def test_allows_email_hostname_to_be_preserved(self):
        filter = PrivacyFilter('someone@example.com')
        filter.preserve_email_hostname = True
        self.assertEqual('[FILTERED]@example.com', filter.filtered())

    def test_allows_email_usernames_to_be_partially_preserved(self):
        filter = PrivacyFilter('someone@example.com')
        filter.partially_preserve_email_username = True
        self.assertEqual('som[FILTERED]@example.com', filter.filtered())

    def test_filters_phone_numbers(self):
        self.assertEqual('Reach me at: [PHONE]', PrivacyFilter('Reach me at: 0885123123').filtered())

    def test_do_not_preserve_local_format_phome_prefix(self):
        filter = PrivacyFilter('Phone: 025551212')
        filter.preserve_phone_country_code = True
        self.assertEqual('Phone: [PHONE]', filter.filtered())

    def test_allows_country_code_to_be_preserved_for_internationally_formatted_phone_numbers(self):
        filter = PrivacyFilter('Phone: +35925551212')
        filter.preserve_phone_country_code = True
        self.assertEqual('Phone: +359 [FILTERED]', filter.filtered())

    def test_do_not_replace_invalid_phone_numbers(self):
        filter = PrivacyFilter("Phone 0012345 is invalid.")
        self.assertEqual("Phone 0012345 is invalid.", filter.filtered())


class ValidationsTest(unittest.TestCase):
    def test_allows_validation_for_emails(self):
        self.assertTrue(Validations.is_email('foo@bar.com'))
        self.assertTrue(Validations.is_email('ivan.bobev@gmail.com'))
        self.assertFalse(Validations.is_email('invalid@email'))
        self.assertFalse(Validations.is_email('ivan.bobev@gmail.com '))

    def test_returns_boolean_True_or_False(self):
        self.assertTrue(isinstance(Validations.is_email('foo@bar.com'), bool))
        self.assertTrue(isinstance(Validations.is_email('invalid@email'), bool))

    def test_validates_phone_numbers(self):
        self.assertTrue(Validations.is_phone('08812121212'))
        self.assertTrue(Validations.is_phone('+359 88 121-212-12'))
        self.assertTrue(Validations.is_phone('+35929555111'))
        self.assertTrue(Validations.is_phone('+359 295 555 111'))
        self.assertTrue(Validations.is_phone('+359  295  555  111'))
        self.assertTrue(Validations.is_phone('0  295-555-111'))
        self.assertTrue(Validations.is_phone('0012345678912345'))
        self.assertTrue(Validations.is_phone('0 00000000000'))
        self.assertTrue(Validations.is_phone('001000000000'))
        self.assertFalse(Validations.is_phone('0  295-555-111 '))
        self.assertFalse(Validations.is_phone('00123456789123456'))
        self.assertFalse(Validations.is_phone('0  2-9-5'))
        self.assertFalse(Validations.is_phone('123123'))
        self.assertFalse(Validations.is_phone('+359  295   555  111'))
        self.assertFalse(Validations.is_phone('000123456'))
        self.assertFalse(Validations.is_phone('+359'))
        self.assertFalse(Validations.is_phone('00000000000'))

    def test_validates_hostnames(self):
        self.assertTrue(Validations.is_hostname('foo.bar.baz'))
        self.assertTrue(Validations.is_hostname('domain.tld'))
        self.assertTrue(Validations.is_hostname("foo-bar.goo.co.uk"))
        self.assertFalse(Validations.is_hostname("foo-bar.goo-.co.uk"))
        self.assertFalse(Validations.is_hostname('not-a-hostname'))

    def test_validates_IP_addresses(self):
        self.assertTrue(Validations.is_ip_address('0.0.0.0'))
        self.assertTrue(Validations.is_ip_address('1.2.3.4'))
        self.assertTrue(Validations.is_ip_address('127.0.0.1'))
        self.assertTrue(Validations.is_ip_address('192.168.67.33'))
        self.assertTrue(Validations.is_ip_address('255.255.255.255'))
        self.assertFalse(Validations.is_ip_address('-127.0.0.1'))
        self.assertFalse(Validations.is_ip_address('127.000.000.001'))
        self.assertFalse(Validations.is_ip_address('1.2..3.4'))
        self.assertFalse(Validations.is_ip_address('1.2.3.4 '))
        self.assertFalse(Validations.is_ip_address('1.2.-3.4'))
        self.assertFalse(Validations.is_ip_address('192.168.256.33'))

    def test_validates_numbers(self):
        self.assertTrue(Validations.is_number('0'))
        self.assertTrue(Validations.is_number('-0'))
        self.assertTrue(Validations.is_number('42'))
        self.assertTrue(Validations.is_number('-42'))
        self.assertTrue(Validations.is_number('100'))
        self.assertTrue(Validations.is_number('-1000000'))
        self.assertFalse(Validations.is_number('001'))
        self.assertFalse(Validations.is_number('12ala'))
        self.assertFalse(Validations.is_number('123 456'))
        self.assertFalse(Validations.is_number('universe'))

        self.assertTrue(Validations.is_number('0.0'))
        self.assertTrue(Validations.is_number('0.000'))
        self.assertTrue(Validations.is_number('-0.000'))
        self.assertTrue(Validations.is_number('42'))
        self.assertTrue(Validations.is_number('42.42'))
        self.assertTrue(Validations.is_number('0.00015'))
        self.assertFalse(Validations.is_number('x'))
        self.assertFalse(Validations.is_number('0.'))
        self.assertFalse(Validations.is_number('.5'))
        self.assertFalse(Validations.is_number('00.000'))

    def test_validates_integers(self):
        self.assertTrue(Validations.is_integer('0'))
        self.assertTrue(Validations.is_integer('-0'))
        self.assertTrue(Validations.is_integer('42'))
        self.assertTrue(Validations.is_integer('-42'))
        self.assertTrue(Validations.is_integer('100'))
        self.assertFalse(Validations.is_integer('01'))
        self.assertFalse(Validations.is_integer('001'))
        self.assertFalse(Validations.is_integer('12ala'))
        self.assertFalse(Validations.is_integer('123 456'))
        self.assertFalse(Validations.is_integer('universe'))

    def test_validates_dates(self):
        self.assertTrue(Validations.is_date('2012-11-19'))
        self.assertTrue(Validations.is_date('2013-01-01'))
        self.assertTrue(Validations.is_date('2014-02-31'))
        self.assertTrue(Validations.is_date('0917-08-20'))
        self.assertTrue(Validations.is_date('0000-02-25'))
        self.assertTrue(Validations.is_date('0001-02-25'))
        self.assertFalse(Validations.is_date('2014-00-25'))
        self.assertFalse(Validations.is_date('25-02-25'))
        self.assertFalse(Validations.is_date('1995-4-31'))
        self.assertFalse(Validations.is_date('2014-31-02'))
        self.assertFalse(Validations.is_date('-0917-08-20'))
        self.assertFalse(Validations.is_date('0917-08-20-'))
        self.assertFalse(Validations.is_date('Jamaica'))

    def test_validates_times(self):
        self.assertTrue(Validations.is_time('12:00:00'))
        self.assertTrue(Validations.is_time('12:00:01'))
        self.assertTrue(Validations.is_time('00:00:00'))
        self.assertTrue(Validations.is_time('06:06:06'))
        self.assertTrue(Validations.is_time('23:59:59'))
        self.assertFalse(Validations.is_time('23:59:59:'))
        self.assertFalse(Validations.is_time(':23:59:59'))
        self.assertFalse(Validations.is_time('24:00:00'))
        self.assertFalse(Validations.is_time('23:60:00'))
        self.assertFalse(Validations.is_time('2:00:00'))
        self.assertFalse(Validations.is_time('not a time'))

    def test_validates_datetime_values(self):
        self.assertTrue(Validations.is_datetime('2012-11-19 19:00:00'))
        self.assertTrue(Validations.is_datetime('2012-11-19T19:15:00'))
        self.assertFalse(Validations.is_datetime('2012-11-19 T 19:15:00'))
        self.assertFalse(Validations.is_datetime('19:00:00 2012-11-19'))
        self.assertFalse(Validations.is_datetime('2012-11-19 24:00:00'))
        self.assertFalse(Validations.is_datetime('12-11-19 00:00:00'))
        self.assertFalse(Validations.is_datetime('foo'))


if __name__ == '__main__':
    unittest.main()
