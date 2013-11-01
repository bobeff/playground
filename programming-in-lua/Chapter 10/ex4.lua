--[[
    Generalize the Markov-chain algorithm so that it can use any size for
the sequence of previous words used in the choice of the next word.
]]

local INPUT_FILE_NAME    = arg[1]
local PREFIX_WORDS_COUNT = arg[2] or 2
local TEXT_WORDS_COUNT   = arg[3] or 100
local NO_WORD = "\n"

local prefixWords;

local function initPrefixWords(t)
    for i = 1, PREFIX_WORDS_COUNT do
        t[i] = NO_WORD
    end
end

local function prefix(t)
    local res = t[1]
    for i = 2, #t do
        res = res .. " " .. t[i]
    end
    return res
end

local function rotatePrefix(t, w)
    for i = 1, #t - 1 do
        t[i] = t[i + 1]
    end
    t[#t] = w
end

local function allwords(fileName)
    local file = io.open(fileName, "r")
    local f = function()
        for line in file:lines() do
            for w in string.gmatch(line, "%w+") do
                coroutine.yield(w)
            end
        end
    end
    return coroutine.wrap(f)
end

local words = {}

local function insert(prefix, word)
    local list = words[prefix]
    if not list then
        words[prefix] = { word }
    else
        table.insert(list, word)
    end
end

if #arg < 1 then
    print("Usage: " .. arg[0] ..
          " <file_name> [<prefix_count> = 2] [<text_words_count> = 100]")
end

local prefixWords = {}
initPrefixWords(prefixWords)

for w in allwords(INPUT_FILE_NAME) do
    insert(prefix(prefixWords), w)
    rotatePrefix(prefixWords, w)
end
insert(prefix(prefixWords), NO_WORD)

--[[
for k, v in pairs(words) do
    io.write(k .. " -> ")
    for i = 1, #v do
        io.write(v[i] .. ", ")
    end
    io.write("\n")
end
]]

initPrefixWords(prefixWords)
for i = 1, TEXT_WORDS_COUNT do
    local list = words[prefix(prefixWords)]
    local w = math.random(#list)
    local word = list[w]
    if word == NO_WORD then return end
    io.write(word, " ")
    rotatePrefix(prefixWords, word)
end
