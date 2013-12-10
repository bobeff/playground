function foo(x, y)
    print(string.format("foo(%f, %f)", x, y))
    return math.abs(x - y) < 0.00001
end

function bar(x, y)
    print(string.format("bar(%d, %d)", x, y))
    return x + y, x - y
end
