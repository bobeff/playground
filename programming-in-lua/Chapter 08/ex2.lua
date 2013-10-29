--[[
    Write a function "multiload" that generalizes "loadwithprefix" by
receiving a list of readers.
]]

function multiload(...)
    local t = {...}
    local index = 0

    local function getreader()
        return function()
            local value

            repeat
                index = index + 1
                local element = t[index]
                local elementType = type(element)

                if elementType == "string" then
                    value = element
                elseif elementType == "function" then
                    value = element()
                    if value then index = index - 1 end
                elseif elementType == "nil" then
                    return nil
                else
                    error("Argument must be string or reader function.", 3)
                end
            until value or index > #t

            return value
        end
    end

    return load(getreader())
end

f = assert(multiload("local x = 10;", io.lines("temp.lua", "*L"), " print(x)"))
f()
