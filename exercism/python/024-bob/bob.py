def response(hey_bob):
    hey_bob = hey_bob.strip()

    if len(hey_bob) == 0:
        return "Fine. Be that way!"

    is_question = hey_bob[-1] == '?'
    is_yelling = hey_bob.isupper()

    if is_question and not is_yelling:
        return "Sure."
    elif is_question and is_yelling:
        return "Calm down, I know what I'm doing!"
    elif not is_question and is_yelling:
        return "Whoa, chill out!"
    else:
        return "Whatever."
