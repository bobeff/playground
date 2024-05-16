def find(search_list, value):
    low = 0
    high = len(search_list) - 1
    while low <= high:
        middle = (low + high) // 2
        if search_list[middle] == value:
            return middle
        elif search_list[middle] < value:
            low = middle + 1
        else:
            high = middle - 1
    raise ValueError('value not in array')
