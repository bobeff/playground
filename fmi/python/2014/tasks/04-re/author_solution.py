import re

USERNAME = '([a-z0-9][\w_\-+\.]{,200})@'
EMAIL = (r'\b(?P<username>[a-z0-9][\w_\-+\.]{,200})@'
         r'(?P<hostname>[a-zA-Z0-9._%-]+\.[a-zA-Z]{2,6})\b')
IP = ('((\d|\d\d|1\d\d|2([0-4]\d|5[0-5]))\.){3}'
      '(\d|\d\d|1\d\d|2([0-4]\d|5[0-5]))\Z')
COUNTRY_CODE = '[1-9]\d{,2}'
PHONE_PREFIX = ('((\b|(?<![\+\w]))0(?!0))|'
                r'((\b00%s|\+%s))' %
                (COUNTRY_CODE, COUNTRY_CODE))
PHONE_NUMBER = r'[ \-\(\)]{,2}(\d[ \-\(\)]{,2}){6,10}\d)\b'
PHONE = ('(?P<prefix>%s)(?P<number>%s' %
         (PHONE_PREFIX, PHONE_NUMBER))
NUMBER = '^-?(0|[1-9]\d*)(\.[0-9]+)?\Z'
INTEGER = '^-?(0|[1-9]\d*)\Z'
HOSTNAME = (r'^([a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?\.)+'
            r'[a-zA-Z]{2,6}\b')
DATE = (r'(\d{4})[- /.](0[1-9]|1[012])[- /.]'
        '(0[1-9]|[12][0-9]|3[01])')
TIME = '([0-1][0-9]|[2][0-3]):([0-5][0-9]):([0-5][0-9])'
DATETIME = ('((?P<date>%s)[ T]'
            '(?P<time>%s))' % (DATE, TIME))


class PrivacyFilter:

    preserve_phone_country_code = False
    preserve_email_hostname = False
    partially_preserve_email_username = False

    def __init__(self, text):
        self.text = text

    def __filter_phone_number(self, text):
        if self.preserve_phone_country_code:
            return self.__preserve_phone_country_code(text)
        else:
            return self.__filtered_phone_number(text)

    def __preserve_phone_country_code(self, text):
        phone = re.search(PHONE, text)
        if len(phone.group('prefix')) >= 2:
            return text.replace(phone.group('number'),  ' [FILTERED]')
        else:
            return text.replace(phone.group(0), '[FILTERED]')

    def __filtered_phone_number(self, text):
        if re.search(PHONE, text):
            return re.sub(PHONE, '[PHONE]', text)

    def __filter_email(self, text):
        if self.preserve_email_hostname:
            return self.__filter_preserve_email_hostname(text)
        elif self.partially_preserve_email_username:
            return self.__filter_partially_preserve_email_username(text)
        else:
            return re.sub(EMAIL, '[EMAIL]', text)

    def __filter_preserve_email_hostname(self, text):
        return re.sub(USERNAME, "[FILTERED]@", text)

    def __filter_partially_preserve_email_username(self, text):
        email = re.search(EMAIL, text)
        if len(email.group('username')) >= 6:
            return text.replace(email.group('username')[3:], "[FILTERED]")
        else:
            return self.__filter_preserve_email_hostname(text)

    def filtered(self):
        if re.search(PHONE, self.text):
            return self.__filter_phone_number(self.text)
        elif re.search(EMAIL, self.text):
            return self.__filter_email(self.text)
        else:
            return self.text


class Validations:

    @classmethod
    def is_email(cls, value):
        return bool(re.match("%s\Z" % EMAIL, value))

    @classmethod
    def is_phone(cls, value):
        return bool(re.match("^%s\Z" % PHONE, value))

    @classmethod
    def is_hostname(cls, value):
        return bool(re.match("%s\Z" % HOSTNAME, value))

    @classmethod
    def is_ip_address(cls, value):
        return bool(re.match(IP, value))

    @classmethod
    def is_number(cls, value):
        return bool(re.match(NUMBER, value))

    @classmethod
    def is_integer(cls, value):
        return bool(re.match(INTEGER, value))

    @classmethod
    def is_date(cls, value):
        return bool(re.match('%s\Z' % DATE, value))

    @classmethod
    def is_time(cls, value):
        return bool(re.match('%s\Z' % TIME, value))

    @classmethod
    def is_datetime(cls, value):
        return bool(re.match('%s\Z' % DATETIME, value))
