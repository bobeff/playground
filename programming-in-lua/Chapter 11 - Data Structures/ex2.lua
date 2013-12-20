--[[
    When we apply the word-frquency program toa text, usually the most
frequent words are uninteresting small words like articles and prepositions.
Change the program so that it ignores words with less then four letters.
]]

local PROGRAM_NAME = arg[0]
local TEXT_FILE_NAME = arg[1]
local IGNORED_WORDS_FILE_NAME = arg[2]
local OUTPUT_WORDS_COUNT = tonumber(arg[3])

function allwords(fileName, ignoredWords)
    local file = fileName and io.open(fileName, "r") or io.stdin
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
                if not ignoredWords or not ignoredWords[word] then
                    return word
                end
            end
        end
    end
end

function buildIgnoredWordsSet()
    if not IGNORED_WORDS_FILE_NAME then
        return {}
    end
    local ignored = {}
    local file = io.open(IGNORED_WORDS_FILE_NAME, "r")
    if not file then return {} end
    for line in file:lines() do
        ignored[line] = true
    end
    return ignored
end

if not arg[1] then
    print("Usage: " .. PROGRAM_NAME ..
          " [<file_name>] [<ignored_words_file_name>] [<words_count>]")
    return
end

local ignoredWords = buildIgnoredWordsSet()
local wordCount = {}
local words = {}

for word in allwords(TEXT_FILE_NAME, ignoredWords) do
    wordCount[word] = (wordCount[word] or 0) + 1
end

for word in pairs(wordCount) do
    table.insert(words, word)
end

table.sort(words, function(x, y)
    return wordCount[x] > wordCount[y] or
           wordCount[x] == wordCount[y] and x < y
end)

for i = 1, OUTPUT_WORDS_COUNT or #words do
    print(words[i], wordCount[words[i]])
end
