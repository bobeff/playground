--[[
    Exercise 13.1: Define a metamethod __sub for sets that returns the
difference of two sets.
    Exercise 13.2: Define a metamethod __len for sets so that #s returns
the number of elements in set s.
]]

Set = {}

local size = {}

function Set.intersect(a, b)
    local res = Set.new{}
    for k in pairs(a) do
        if k ~= size then
            res[k] = b[k]
            res[size] = res[size] + 1
        end
    end
    return res
end

function Set.union(a, b)
    local res = Set.new{}
    for k in pairs(a) do
        if k ~= size then
            res[k] = true
            res[size] = res[size] + 1
        end
    end
    for k in pairs(b) do
        if k ~= size then
            res[k] = true
            res[size] = res[size] + 1
        end
    end
    return res
end

function Set.difference(a, b)
    local res = Set.new{}
    for k in pairs(a) do
        if k ~= size and not b[k] then
            res[k] = true
            res[size] = res[size] + 1
        end
    end
    return res
end

function Set.symetric_difference(a, b)
    return (a - b) + (b - a)
end

function Set.size(a)
    return a[size]
end

function Set.subset(a, b)
    for k in pairs(a) do
        if k ~= size and not b[k] then
            return false
        end
    end
    return true
end

function Set.tostring(s)
    local str = {}
    for k in pairs(s) do
        if k ~= size then
            str[#str + 1] = k
        end
    end
    return "{ " .. table.concat(str, ", ") .. " }"
end

function Set.add(s, x)
    if not s[x] then
        s[x] = true
        s[size] = s[size] + 1
        return true
    end
    return false
end

local mt =
{
    __metatable = "protected",
    __tostring  = Set.tostring,
    __add = Set.union,
    __sub = Set.difference,
    __mul = Set.intersect,
    __len = Set.size,
    __le  = Set.subset,
    __lt  = function(a, b)
        return a <= b and not (b <= a)
    end,
    __eq  = function(a, b)
        return a <= b and b <= a
    end,
}

function Set.new(list)
    local set = {}
    setmetatable(set, mt)
    set[size] = #list
    for _, v in ipairs(list) do
       set[v] = true
    end
    return set
end

local s1 = Set.new{5, 10, 15}
print("s1 = ", s1)

local s2 = Set.new{3, 10, 13}
print("s2 = ", s2)
print("s1 + s2 = ", s1 + s2)
print("s1 * s2 = ", s1 * s2)
print("s1 - s2 = ", s1 - s2)
print("s2 - s1 = ", s2 - s1)
print("s1 -+- s2 ", Set.symetric_difference(s1, s2))

local s3 = Set.new{3, 13}
print("s3 = ", s3)
print("s3 < s2 = ", s3 < s2)
print("s2 < s3 = ", s2 < s3)

local s4 = Set.new{5, 10, 15}
print("s4 = ", s4)
print("s1 < s4 =", s1 < s4)
print("s1 <= s4 = ", s1 <= s4)
print("s1 == s4 = ", s1 == s4)
print("s1 ~= s4 = ", s1 ~= s4)
print("s1 ~= s2 = ", s1 ~= s2)
