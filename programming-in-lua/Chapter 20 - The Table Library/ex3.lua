--[[
    Write a function to test whether a given table is a valid sequence.
]]

function issequence(t)
    local pairsCount = 0
    for k, v in pairs(t) do
        pairsCount = pairsCount + 1
    end

    local ipairsCount = 0
    for i, v in ipairs(t) do
        ipairsCount = ipairsCount + 1
    end

    return pairsCount == ipairsCount
end

print(issequence({1, 2, 3}))
print(issequence({1, 2, nil, 3}))
print(issequence({a = 4, 1, 2, 3}))
