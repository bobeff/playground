import unittest

import my_solution as solution


class PrivacyFilterTest(unittest.TestCase):

    def filter_email_usernames(self, text):
        filter = solution.PrivacyFilter(text)
        filter.preserve_email_hostname = True
        return filter.filtered()

    def partially_filter_email_usernames(self, text):
        filter = solution.PrivacyFilter(text)
        filter.partially_preserve_email_username = True
        return filter.filtered()

    def test_works_with_blank_or_whitespace_strings_and_preserves_whitespace(self):
        self.assertEqual('', solution.PrivacyFilter('').filtered())
        self.assertEqual('   ', solution.PrivacyFilter('   ').filtered())
        self.assertEqual(" \n \n ", solution.PrivacyFilter(" \n \n ").filtered())

    def test_obfuscates_simple_emails(self):
        self.assertEqual('Contact: [EMAIL]', solution.PrivacyFilter('Contact: someone@example.com').filtered())

    def test_obfuscates_more_complicated_emails(self):
        complicated_emails = {
            'some.user+and-more-here@gmail.com': '[EMAIL]',
            'some.user+and-more-here@nihon.co.jp': '[EMAIL]',
            'some.user+and-more-here@lawn.co.uk': '[EMAIL]',
            'larodi@x.com': '[EMAIL]',
            'xyz@sunny.com.br': '[EMAIL]',
            'Contact:someone@example.com': 'Contact:[EMAIL]',
            'Contact: 1someone@example.com,someone.new@sub.example123.co.uk': 'Contact: [EMAIL],[EMAIL]',
        }
        for text, filtered in complicated_emails.items():
            self.assertEqual(filtered, solution.PrivacyFilter(text).filtered())

    def test_does_not_filter_invalid_emails(self):
        text_with_invalid_emails = [
            'Contact me here: _invalid@email.com',
            'And more: someone@invalid.domaintld',
            'someone@invalid.domaintld',
            'someone@invalid.domaint.l.d',
            'Whaa? -@example.com',
        ]
        for text in text_with_invalid_emails:
            self.assertEqual(text, solution.PrivacyFilter(text).filtered())
            self.assertEqual(text, solution.PrivacyFilter(text).filtered())
            self.assertEqual(text, self.filter_email_usernames(text))
            self.assertEqual(text, self.partially_filter_email_usernames(text))

    def test_allows_email_hostname_to_be_preserved(self):
        self.assertEqual('[FILTERED]@example.com', self.filter_email_usernames('someone@example.com'))
        self.assertEqual('[FILTERED]@exa.mple.com', self.filter_email_usernames('some12-+3@exa.mple.com'))

    def test_allows_email_usernames_to_be_partially_preserved(self):
        self.assertEqual('som[FILTERED]@example.com', self.partially_filter_email_usernames('someone@example.com'))

    def test_filters_whole_email_usernames_if_too_short(self):
        self.assertEqual('[FILTERED]@example.com', self.partially_filter_email_usernames('me@example.com'))

    def test_does_not_brake_with_unicode(self):
        self.assertEqual('За връзка: [FILTERED]@example.com', self.partially_filter_email_usernames('За връзка: me@example.com'))

    def test_filters_more_complex_phone_numbers(self):
        more_complex_phone_numbers = {
            'Reach me at: 0885123123': 'Reach me at: [PHONE]',
            '+155512345699': '[PHONE]',
            '+1 555 123-456': '[PHONE]',
            '+1 (555) 123-456-99': '[PHONE]',
            '004412125543': '[PHONE]',
            '0044 1 21 25 543': '[PHONE]',
        }
        for text, filtered in more_complex_phone_numbers.items():
            self.assertEqual(filtered, solution.PrivacyFilter(text).filtered())

    def test_does_not_filter_invalid_phone_numbers(self):
        invalid_phone_numbers = {
            'Reach me at: 0885123': 'Reach me at: [PHONE]',
            '0005551234569': '0005551234569',
            '+1555 123, 55555': '+1555 123, 55555',
            '95551212': '95551212',
        }
        for text, filtered in invalid_phone_numbers.items():
            self.assertEqual(filtered, solution.PrivacyFilter(text).filtered())

    def test_preserves_whitespace_around_phones(self):
        self.assertEqual(' [PHONE] or...', solution.PrivacyFilter(' +359881212-12-1 2 or...').filtered())

    def test_filters_more_than_one_phone_or_email(self):
        text = """
        Contacts

        Phones:_+1_(555) 123-456-99 or 004412125543
        Email: contact@company.co.uk or sales@office.us
        """

        filtered = """
        Contacts

        Phones:_+1_(555) 123-456-99 or [PHONE]
        Email: [EMAIL] or [EMAIL]
        """

        self.assertEqual(filtered, solution.PrivacyFilter(text).filtered())

    def test_allows_country_code_to_be_preserved_for_internationally_formatted_phone_numbers(self):
        internationally_formatted_phone_numbers = {
            'Phone: +359 2 555-1212': 'Phone: +359 [FILTERED]',
            'Phone: +35925551212': 'Phone: +359 [FILTERED]',
            'Phone: 08825551212': 'Phone: [PHONE]',
            'Phone: 0 88 255-512 12 !': 'Phone: [PHONE] !',
            'Phone: 0025 5512 12255': 'Phone: 0025 [FILTERED]',
        }
        for text, filtered in internationally_formatted_phone_numbers.items():
            filter = solution.PrivacyFilter(text)
            filter.preserve_phone_country_code = True
            self.assertEqual(filtered, filter.filtered())

    def test_separates_preserved_country_code_from_filtered_phone_with_a_space(self):
        filtered_phones_with_a_space = {
            'Phone: 0025 (55) 12 12255': 'Phone: 0025 [FILTERED]',
            'Phone: 0025(55) 12 12255': 'Phone: 0025 [FILTERED]',
            'Phone: +25( 55 )12 12255': 'Phone: +25 [FILTERED]',
        }
        for text, filtered in filtered_phones_with_a_space.items():
            filter = solution.PrivacyFilter(text)
            filter.preserve_phone_country_code = True
            self.assertEqual(filtered, filter.filtered())


class ValidationsTest(unittest.TestCase):
    def test_allows_validation_for_emails(self):
        self.assertTrue(solution.Validations.is_email('foo@bar.com'))
        self.assertFalse(solution.Validations.is_email('invalid@email'))

    def test_returns_boolean_True_or_False(self):
        self.assertIs(solution.Validations.is_email('foo@bar.com'), True)
        self.assertIs(solution.Validations.is_email('invalid@email'), False)

    def test_can_validate_more_complex_emails(self):
        more_complex_emails = {
            'someone.else@example.org': True,
            'someone.else+and.some-more@foo.org.uk': True,
            'someone.else+and.some-more@foo.org.invalidtld': False,
            'x@@foo.org': False,
            '_x@foo.org': False,
            '42@universe.com': True,
            ' foo@universe.com': False,
            'foo@universe.com ': False,
        }
        for email, valid in more_complex_emails.items():
            self.assertIs(solution.Validations.is_email(email), valid)

    def test_does_not_break_on_emails_in_multiline_strings(self):
        self.assertFalse(solution.Validations.is_email("foo@bar.com\nwat?"))

    def test_validates_phone_numbers(self):
        self.assertTrue(solution.Validations.is_phone('+35929555111'))
        self.assertFalse(solution.Validations.is_phone('123123'))

    def test_can_validate_more_complex_phone_numbers(self):
        more_complex_phone_numbers = {
            '0885123123': True,
            '+155512345699': True,
            '+1 555 123-456': True,
            '+101 555 123-456-12': True,
            '+1 (555) 123-456-99': True,
            '004412125543': True,
            '0044 1 21 25 543': True,
            ' 0044 1 21 25 543 ': False,
            'Why? 0044 1 21 25 543': False,
            '0885123': True,
            '0005551234569': False,
            '+1555 123, 55555': False,
            '95551212': False,
            '+35929555111': True,
            '0040 295551111': True,
            '0896841090': True,
            '0(896) 84-10-90': True,
            '+359 89 684-109-05': True,
            '123123': False,
            '0896 - 841090': False,
            '+896 701 841 090 808': False,
            '+896 701 841 090 ': False,
            '+0896 701 841 090': False,
        }
        for phone, valid in more_complex_phone_numbers.items():
            self.assertIs(solution.Validations.is_phone(phone), valid)

    def test_does_not_break_on_phones_in_multiline_strings(self):
        self.assertFalse(solution.Validations.is_phone("0885123123\nwat?"))

    def test_validates_hostnames(self):
        self.assertTrue(solution.Validations.is_hostname('domain.tld'))
        self.assertTrue(solution.Validations.is_hostname('some.long-subdomain.domain.co.ul'))
        self.assertFalse(solution.Validations.is_hostname('localhost'))
        self.assertTrue(solution.Validations.is_hostname('1.2.3.4.xip.io'))
        self.assertTrue(solution.Validations.is_hostname('x.io'))
        self.assertTrue(solution.Validations.is_hostname('sub0domain.not-a-hostname.com'))
        self.assertTrue(solution.Validations.is_hostname('sub-sub.sub.not123a-hostname.co.uk'))
        self.assertFalse(solution.Validations.is_hostname('not-a-hostname'))
        self.assertFalse(solution.Validations.is_hostname('not-a-hostname-.com'))
        self.assertFalse(solution.Validations.is_hostname('not-a-hostname.c-m'))
        self.assertFalse(solution.Validations.is_hostname('not-a-hostname.com.12'))

    def test_handles_multiline_strings_in_hostname_validation_properly(self):
        self.assertFalse(solution.Validations.is_hostname("foo.com\n"))
        self.assertFalse(solution.Validations.is_hostname("foo.com\nbar.com"))

    def test_validates_IP_addresses(self):
        self.assertTrue(solution.Validations.is_ip_address('1.2.3.4'))
        self.assertFalse(solution.Validations.is_ip_address('300.2.3.4'))
        self.assertTrue(solution.Validations.is_ip_address('0.0.0.0'))
        self.assertTrue(solution.Validations.is_ip_address('255.255.255.255'))

    def test_handles_multiline_strings_in_IP_validation_properly(self):
        self.assertFalse(solution.Validations.is_ip_address("8.8.8.8\n"))
        self.assertFalse(solution.Validations.is_ip_address("\n8.8.8.8"))
        self.assertFalse(solution.Validations.is_ip_address("1.2.3.4\n8.8.8.8"))

    def test_validates_numbers(self):
        self.assertTrue(solution.Validations.is_number('42'))
        self.assertFalse(solution.Validations.is_number('x'))
        self.assertTrue(solution.Validations.is_number('42.42'))
        self.assertTrue(solution.Validations.is_number('9'))

    def test_validates_more_complex_numbers(self):
        self.assertFalse(solution.Validations.is_number(' 42 '))
        self.assertTrue(solution.Validations.is_number('42.5555550555555555'))
        self.assertTrue(solution.Validations.is_number('0.5555550555555555'))
        self.assertTrue(solution.Validations.is_number('-0.5555550555555555'))
        self.assertTrue(solution.Validations.is_number('0'))
        self.assertFalse(solution.Validations.is_number('00'))
        self.assertFalse(solution.Validations.is_number('.42'))
        self.assertTrue(solution.Validations.is_number('0.0'))
        self.assertTrue(solution.Validations.is_number('-0.0'))
        self.assertTrue(solution.Validations.is_number('0.000000'))
        self.assertTrue(solution.Validations.is_number('0.0000001'))
        self.assertTrue(solution.Validations.is_number('-0.0000001'))
        self.assertFalse(solution.Validations.is_number('1.00 00001'))

    def test_handles_multiline_strings_in_numbers_validation_properly(self):
        self.assertFalse(solution.Validations.is_number("42\n24"))
        self.assertFalse(solution.Validations.is_number("\n24.12"))

    def test_validates_integers(self):
        self.assertTrue(solution.Validations.is_integer('42'))
        self.assertFalse(solution.Validations.is_integer('universe'))

    def test_validates_more_complex_integers(self):
        self.assertFalse(solution.Validations.is_integer(' 42 '))
        self.assertFalse(solution.Validations.is_integer('-42 '))
        self.assertFalse(solution.Validations.is_integer('00'))
        self.assertTrue(solution.Validations.is_integer('0'))
        self.assertTrue(solution.Validations.is_integer('9'))
        self.assertTrue(solution.Validations.is_integer('-0'))
        self.assertFalse(solution.Validations.is_integer('--2132'))
        self.assertTrue(solution.Validations.is_integer('-10000000000000'))

    def test_handles_multiline_strings_in_integer_validation_properly(self):
        self.assertFalse(solution.Validations.is_number("42\n24"))
        self.assertFalse(solution.Validations.is_number("\n24\n"))

    def test_validates_dates(self):
        self.assertTrue(solution.Validations.is_date('2012-11-19'))
        self.assertFalse(solution.Validations.is_date(' '))
        self.assertFalse(solution.Validations.is_date(''))
        self.assertFalse(solution.Validations.is_date('Jamaica'))

    def test_allows_zero_years_in_date_validation(self):
        self.assertTrue(solution.Validations.is_date('0000-01-01'))

    def test_allows_huge_years_in_date_validation(self):
        self.assertTrue(solution.Validations.is_date('9999-01-01'))

    def test_does_not_allow_zero_months_or_days_in_dates(self):
        self.assertFalse(solution.Validations.is_date('1000-00-01'))
        self.assertFalse(solution.Validations.is_date('1000-01-00'))
        self.assertFalse(solution.Validations.is_date('2012-00-00'))

    def test_does_not_allow_invalid_months_or_days_in_dates(self):
        self.assertFalse(solution.Validations.is_date('2012-13-01'))
        self.assertFalse(solution.Validations.is_date('2012-06-32'))
        self.assertFalse(solution.Validations.is_date('2012-06-99'))

    def test_handles_newlines_in_date_validation(self):
        self.assertFalse(solution.Validations.is_date("2012-11-19\n"))
        self.assertFalse(solution.Validations.is_date("2012-11-19\n2012-10-10"))

    def test_validates_times(self):
        self.assertTrue(solution.Validations.is_time('12:00:00'))
        self.assertFalse(solution.Validations.is_time('not a time'))
        self.assertTrue(solution.Validations.is_time('00:00:00'))
        self.assertTrue(solution.Validations.is_time('23:59:59'))
        self.assertFalse(solution.Validations.is_time('3:59:59'))

    def test_does_not_allow_invalid_hours_minutes_or_seconds(self):
        self.assertFalse(solution.Validations.is_time('24:00:00'))
        self.assertFalse(solution.Validations.is_time('12:69:00'))
        self.assertFalse(solution.Validations.is_time('12:01:99'))
        self.assertFalse(solution.Validations.is_time('12:1:9'))
        self.assertFalse(solution.Validations.is_time(' 12:01:09 '))

    def test_validates_datetime_values(self):
        self.assertTrue(solution.Validations.is_datetime('2012-11-19 19:00:00'))
        self.assertTrue(solution.Validations.is_datetime('2012-11-19T19:00:00'))
        self.assertFalse(solution.Validations.is_datetime('foo'))
        self.assertTrue(solution.Validations.is_datetime('9999-11-19T23:59:00'))
        self.assertFalse(solution.Validations.is_datetime('2012-00-19T23:59:00'))
        self.assertFalse(solution.Validations.is_datetime('2012-01-00T23:59:00'))
        self.assertFalse(solution.Validations.is_datetime('2012-01-01T24:59:00'))
        self.assertFalse(solution.Validations.is_datetime('2012-01-01T12:60:00'))
        self.assertFalse(solution.Validations.is_datetime('2012-01-01T12:04:60'))

    def test_handles_newlines_in_time_and_datetime_validation(self):
        self.assertFalse(solution.Validations.is_time("12:01:01\n"))
        self.assertFalse(solution.Validations.is_time("12:01:01\n12:02:02"))
        self.assertFalse(solution.Validations.is_datetime("2012-11-19 12:01:01\n"))

if __name__ == '__main__':
    unittest.main()
