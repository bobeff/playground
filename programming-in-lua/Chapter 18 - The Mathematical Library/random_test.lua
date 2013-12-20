random = require "random"

random.initialize(os.time())

-- test exercise 18.2 --------------------------------------------------------

for i = 1, 10 do
    print(random.uniform())
end

print()

for i = 1, 10 do
    print(random.uniform(10))
end

print()

for i = 1, 10 do
    print(random.uniform(10, 11))
end

-- test exercise 18.3 --------------------------------------------------------

print()
print("Normal distribution:")
print()

local function round(x)
    return math.floor(x + 0.5)
end

local t = { size = 50 }
for i = 1, t.size do
    t[i] = 0
end

for i = 1, 100000 do
    local u, v = random.normal()
    local index = round(t.size / 12 * (u + 6))
    t[index] = t[index] + 1
    index = round(t.size / 12 * (v + 6))
    t[index] = t[index] + 1
end

for i = 1, t.size do
    local str = string.rep("*", round(t[i] / 1000))
    print(i, str)
end

-- test exercise 18.4 --------------------------------------------------------

print()
print("Shuffle numbers between 1 and 10:")
print()

local a = {}

for i = 1, 10 do
    a[i] = i
end

random.shuffle(a)

for i = 1, #a do
    print(a[i])
end
