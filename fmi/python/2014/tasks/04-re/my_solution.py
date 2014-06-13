import re


USERNAME_REGEX = r'\b(?P<username>[a-zA-Z0-9][a-zA-Z0-9_+.-]{,200})'
DOMAIN_REGEX = r'([0-9a-zA-Z][0-9a-zA-Z-]{,62}(?<!-)[.])+'
TLD_REGEX = r'[a-zA-Z]{2,3}([.][a-zA-Z]{2})?'
HOSTNAME_REGEX = r'(?P<hostname>' + DOMAIN_REGEX + TLD_REGEX + r')\b'
EMAIL_REGEX = USERNAME_REGEX + r'@' + HOSTNAME_REGEX
PHONE_PREFIX_REGEX = r'(\b0(?!0)|(?P<country_code>(\b00|[+])(?!0)\d{1,3}))'
PHONE_MAIN_PART_REGEX = r'(?P<main_part>([ ()-]{,2}\d+)+)\b'
PHONE_REGEX = PHONE_PREFIX_REGEX + PHONE_MAIN_PART_REGEX
INTEGER_REGEX = r'-?(0|[1-9]\d*)'
FRACTION_REGEX = r'([.]\d+)?'
NUMBER_REGEX = INTEGER_REGEX + FRACTION_REGEX
IP_ADDRESS_REGEX = r'(\d+)[.](\d+)[.](\d+)[.](\d+)'
DATE_REGEX = r'(\d{4})-(\d{2})-(\d{2})'
TIME_REGEX = r'(\d{2}):(\d{2}):(\d{2})'
DATETIME_SEPARATOR_REGEX = r' |T'


def full_match(regex, string):
    match_object = re.match(regex, string)
    if match_object and len(match_object.group(0)) == len(string):
        return match_object


class PrivacyFilter:
    def __init__(self, text):
        self.preserve_phone_country_code = False
        self.preserve_email_hostname = False
        self.partially_preserve_email_username = False
        self.__text = text

    def __email_replace(self, match_object):
        username = match_object.group('username')
        hostname = match_object.group('hostname')
        if self.partially_preserve_email_username and len(username) >= 6:
            return username[:3] + '[FILTERED]@' + hostname
        elif (self.preserve_email_hostname or
              self.partially_preserve_email_username):
            return '[FILTERED]@' + hostname
        else:
            return '[EMAIL]'

    def __phone_replace(self, match_object):
        phone = match_object.group(0)
        if not Validations.is_phone(phone):
            return phone
        phone_main_part = match_object.group('main_part')
        country_code = match_object.group('country_code')
        if self.preserve_phone_country_code and country_code:
            return country_code + ' [FILTERED]'
        else:
            return '[PHONE]'

    def filtered(self):
        filtered_text = re.sub(EMAIL_REGEX, self.__email_replace, self.__text)
        return re.sub(PHONE_REGEX, self.__phone_replace, filtered_text)


class Validations:
    @classmethod
    def is_email(cls, value):
        return bool(full_match(EMAIL_REGEX, value))

    @classmethod
    def is_phone(cls, value):
        phone = full_match(PHONE_REGEX, value)
        if not phone:
            return False
        phone_main_part = phone.group('main_part')
        return 6 <= sum(1 for c in phone_main_part if c.isdigit()) <= 11

    @classmethod
    def is_hostname(cls, value):
        return bool(full_match(HOSTNAME_REGEX, value))

    @classmethod
    def is_ip_address(cls, value):
        ip = full_match(IP_ADDRESS_REGEX, value)
        if not ip:
            return False
        return all(Validations.is_integer(number) and 0 <= int(number) <= 255
                   for number in ip.groups())

    @classmethod
    def is_number(cls, value):
        return bool(full_match(NUMBER_REGEX, value))

    @classmethod
    def is_integer(cls, value):
        return bool(full_match(INTEGER_REGEX, value))

    @classmethod
    def is_date(cls, value):
        date = full_match(DATE_REGEX, value)
        return bool(date) and \
            1 <= int(date.group(2)) <= 12 and \
            1 <= int(date.group(3)) <= 31

    @classmethod
    def is_time(cls, value):
        time = full_match(TIME_REGEX, value)
        return bool(time) and \
            int(time.group(1)) < 24 and \
            int(time.group(2)) < 60 and \
            int(time.group(3)) < 60

    @classmethod
    def is_datetime(cls, value):
        datetime = re.split(DATETIME_SEPARATOR_REGEX, value)
        return len(datetime) == 2 and \
            Validations.is_date(datetime[0]) and \
            Validations.is_time(datetime[1])
