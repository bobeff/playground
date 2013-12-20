--[[
    Exrcise 18.3: Implement a better random function in Lua. Search the web for
good algorithm.
]]

-- http://en.wikipedia.org/wiki/Mersenne_twister#Pseudocode

local _and = bit32.band
local xor = bit32.bxor
local shr = bit32.rshift
local shl = bit32.lshift
local extract = bit32.extract
local log = math.log
local floor = math.floor

_ENV = nil

local MT = {}
local index = 0
local RAND_MAX = 2 ^ 32 - 1

for i = 0, 623 do
    MT[i] = 0
end

local function generate_numbers()
    for i = 0, 623 do
        local y = _and(MT[i], 0x80000000) + _and(MT[(i + 1) % 624], 0x7fffffff)
        MT[i] = xor(MT[(i + 397) % 624], shr(y, 1))
        if y % 2 ~= 0 then
            MT[i] = xor(MT[i], 0x9908b0df)
        end
    end
end

local function initialize(seed)
    index = 0
    MT[0] = seed
    for i = 1, 623 do
        MT[i] = extract(0x6c078965 * (xor(MT[i - 1], shr(MT[i - 1], 30) + i)),
                        0, 32)
    end
end


local function extract_number()
    if index == 0 then
        generate_numbers()
    end
    local y = MT[index]
    y = xor(y, shr(y, 11))
    y = xor(y, _and(shl(y, 7), 0x9d2c5680))
    y = xor(y, _and(shl(y, 15), 0xefc60000))
    y = xor(y, shr(y, 18))
    index = (index + 1) % 624
    return y
end

local function round(x)
    return floor(x + 0.5)
end

local function clamp(x, lower, upper)
    if x < lower then
        return lower
    elseif x > upper then
        return upper
    else
        return x
    end
end

local function uniform(m, n)
    if not m then
        return extract_number() / RAND_MAX
    elseif not n then
        local num = extract_number()
        local res = clamp(round(num / RAND_MAX * m) + 1, 1, m)
        return res
    else
        if m > n then
            error("the first argument must be smaller by the second one.", 2)
        end
        return round(extract_number() / RAND_MAX  * (n - m)) + m
    end
end

--[[
    Exercise 18.4: Using math.random, write a function to produce a
pseudo-random number with a standartd normal (Gaussian) distribution.
]]

-- http://www.design.caltech.edu/erik/Misc/Gaussian.html

local function normal(mean, stddev)
    mean = mean or 0
    stddev = stddev or 1
    local w, x1, x2
    repeat
        x1 = 2 * uniform() - 1
        x2 = 2 * uniform() - 1
        w = x1 * x1 + x2 * x2
    until w < 1
    w = (-2 * log(w) / w) ^ 0.5
    local y1 = mean + stddev * x1 * w
    local y2 = mean + stddev * x2 * w
    return y1, y2
end

--[[
    Exercise 18.5: Write a function to shuffle a given list. Make shure that
all permutations are equally probable.
]]

local function shuffle(array)
    for i = #array, 2, -1 do
        local index = uniform(i)
        array[i], array[index] = array[index], array[i]
    end
end

return
{
    initialize = initialize,
    uniform    = uniform,
    normal     = normal,
    shuffle    = shuffle,
}
