--[[
    Rewrite function rconcat so that it can get a separato, just like
table.concat does.
]]

function rconcat(l, sep)
    if type(l) ~= "table" then return l end
    local res = {}
    for i = 1, #l do
        local ret = rconcat(l[i], sep)
        if ret ~= "" then
            res[#res + 1] = ret
        end
    end
    return table.concat(res, sep)
end

print(rconcat({{{"a", "b"}, {"c"}}, "d", {}, {"e"}}, ";"))
print(rconcat({{"a", {"nice"}}, "and", {{"long"}, {"list"}}}, " "))
