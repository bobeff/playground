--[[
    Frequently, it is useful to add some prefix to a chunk of code when
loading it. Write a function "loadwithprefix" that works like load, except
thar it adds its extra first argument (a string) as a prefix to the chunk
being loaded.
    Like the original "load", "loadwithprefix" should accept chunks
represented both as strings and as reader functions. Even in the case that
original chunk is a string, "loadwithprefix" should not actually concatenate
the prefix with the chunk. Instead, it should call "load" with proper reader
function that first returns the prefix and then return the original chunk.
]]

function loadwithprefix(prefix, chunk)

    local chunktype = type(chunk)
    if chunktype ~= "string" and chunktype ~= "function" then
         error("Second argument must be either string \z
                or reader function", 2);
    end

    local times = 0

    local function getreader()
        return function()
            times = times + 1

            if prefix and prefix ~= "" and times == 1 then
                return prefix
            end

            if chunktype == "string" and times < 3 then
                return chunk
            elseif chunktype == "function" then
                return chunk()
            else
                return nil
            end
        end
    end

    return load(getreader())
end

print("Expression: ")
local l = io.read()
local f1 = assert(loadwithprefix("return ", l))
print("Value: " .. f1())

f2 = assert(loadwithprefix("", io.lines("test.lua", "*L")))
f2()

testFile = io.open("test.lc", "rb")
f3 = assert(loadwithprefix(nil, testFile:read("*all")))
f3()

f4 = assert(loadwithprefix("return ", {1}))
