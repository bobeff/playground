BRACKETS = {'(': ')', '[': ']', '{': '}'}


def is_paired(input_string):
    stack = []
    for c in input_string:
        if c in BRACKETS.keys():
            stack.append(c)
        elif c in BRACKETS.values():
            if len(stack) == 0 or BRACKETS[stack.pop()] != c:
                return False
    return len(stack) == 0
