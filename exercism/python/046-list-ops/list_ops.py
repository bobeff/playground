def append(list1, list2):
    for item in list2:
        list1.append(item)
    return list1


def concat(lists):
    result = []
    for list in lists:
        for item in list:
            result.append(item)
    return result


def filter(function, list):
    result = []
    for item in list:
        if function(item):
            result.append(item)
    return result


def length(list):
    result = 0
    for item in list:
        result += 1
    return result


def map(function, list):
    result = []
    for item in list:
        result.append(function(item))
    return result


def foldl(function, list, initial):
    result = initial
    for item in list:
        result = function(result, item)
    return result


def foldr(function, list, initial):
    result = initial
    for item in reverse(list):
        result = function(result, item)
    return result


def reverse(list):
    result = []
    for i in range(length(list) - 1, -1, -1):
        result.append(list[i])
    return result
