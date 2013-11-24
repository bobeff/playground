--[[
    Write a split function that receives a string and a delimiter pattern and
returns asequencewith the chunks in the original string separated by the
delimiter. How does your funciton handels empty strings ? In particular, is
an empty string an empty sequence or a sequence with one empty string ?
]]

function split(str, pattern)
    local res = {}
    local prev_j = 0
    while true do
        local i, j = str:find(pattern, prev_j + 1)
        if not i then i = 0 end
        local s = str:sub(prev_j + 1, i - 1)
        if s ~= "" then
            res[#res + 1] = s
        end
        if not j then break end
        prev_j = j
    end
    return res
end

local function printtable(t)
    io.write("{ ")
    for _, v in ipairs(t) do
        io.write(v, ", ")
    end
    io.write(" }\n")
end

local t = split("a whole new world", " ")
printtable(t)
t = split(" a whole new world ", " ")
printtable(t)
t = split("", " ")
printtable(t)
