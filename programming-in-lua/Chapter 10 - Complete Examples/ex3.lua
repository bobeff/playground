--[[
    When we apply the word-frquency program toa text, usually the most
frequent words are uninteresting small words like articles and prepositions.
Change the program so that it ignores words with less then four letters.
]]

local programName = arg[0]
local fileName = arg[1]
local minLength = tonumber(arg[2])
local wordsCount = tonumber(arg[3])

function allwords(fileName)
    local file = fileName and io.open(fileName, "r") or io.stdin()
    local line = file:read()
    local pos = 1
    return function()
        while line do
            local b, e = string.find(line, "%w+", pos)
            if not b then
                line = file:read()
            else
                pos = e + 1
                local word = string.sub(line, b, e)
                if not minLength or #word >= minLength then
                    return word
                end
            end
        end
    end
end

if not arg[1] then
    print("Usage: " .. programName ..
          " [<file_name>] [<min_length>] [<words_count>]")
    return
end

local wordCount = {}
local words = {}

for word in allwords(fileName) do
    wordCount[word] = (wordCount[word] or 0) + 1
end

for word in pairs(wordCount) do
    table.insert(words, word)
end

table.sort(words, function(x, y)
    return wordCount[x] > wordCount[y] or
           wordCount[x] == wordCount[y] and x < y
end)

for i = 1, wordsCount or #words do
    print(words[i], wordCount[words[i]])
end
