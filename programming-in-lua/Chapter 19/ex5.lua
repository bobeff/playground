--[[
    Write a function that receives a string coded in UTF-8 and returns its
first code point as a number. The function should return nil if the string
does not start with a valid UTF-8 sequence.
]]

--[[
    UTF-8 encoding:

    00000000-0000007F 0xxxxxxx
    00000080-000007FF 11Oxxxxx 10xxxxxx
    00000800-0000FFFF 1110xxxx 10xxxxxx 10xxxxxx
    00010000-001FFFFF 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    00200000-03FFFFFF 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
    04000000-7FFFFFFF 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
]]

function firstCodePoint(s)

    local function traverseByte(mask, res, char)
        while mask > 0 do
            res = bit32.lshift(res, 1)
            res = res + (bit32.btest(char, mask) and 1 or 0)
            mask = bit32.rshift(mask, 1)
        end
        return res
    end

    local function extractFirst(char, bytes)
        local mask = 2 ^ (8 - bytes - 1)
        local res = 0
        return traverseByte(mask, res, char)
    end

    local function extractNext(res, s, i)
        local mask = 2 ^ 5
        local char = string.byte(s:sub(i, i))
        return traverseByte(mask, res, char)
    end

    local mask = 2 ^ 7
    local bytesCount = 0
    local firstByte = string.byte(s:sub(1, 1))

    while bit32.btest(firstByte, mask) do
        bytesCount = bytesCount + 1
        mask = bit32.rshift(mask, 1)
    end

    if bytesCount == 0 then
   	    return firstByte
    else
        local res = extractFirst(firstByte, bytesCount);
        for i = 2, bytesCount do
            res = extractNext(res, s, i)
        end
        return res
    end
end

print(firstCodePoint("добър ден"))
print(firstCodePoint("good day"))
print(firstCodePoint("יום טוב"))
