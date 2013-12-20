--[[
    Write a function to reverse a UTF-8 string.
]]

function utf8reverse(s)
    local chars = {}
    for char in s:gmatch(".[\128-\191]*") do
        chars[#chars + 1] = char
    end
    local i = 1
    local j = #chars
    while i < j do
        chars[i], chars[j] = chars[j], chars[i]
        i = i + 1
        j = j - 1
    end
    return table.concat(chars)
end

print(utf8reverse("добър ден"))
print(utf8reverse("good day"))
print(utf8reverse("יום טוב"))
