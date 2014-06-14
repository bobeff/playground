EMPTY = ' '


def transpose(matrix):
    return [list(column) for column in zip(*matrix)]


def validate_row(row, row_keys):
    return all(len(filled_group) == key
               for filled_group, key in
               zip(filter(lambda x: x != '', ''.join(row).split(EMPTY)),
                   row_keys))


def validate_nonogram(nonogram, keys):
    lines_for_validation = zip(nonogram + transpose(nonogram),
                               keys['rows'] + keys['columns'])

    return all(validate_row(*line) for line in lines_for_validation)
