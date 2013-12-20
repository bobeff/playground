--[[
    Write a transliterate function. This function receives a string and
replaces each character in that string by another character, according to a
table given as a second argument. If the table maps 'a' to 'b', the function
should replace any occurence of 'a' by 'b'. If the table maps 'a' to false,
the function should remove occurences of 'a' from the resulting string.
]]

function transliterate(s, t)
    for k, v in pairs(t) do
        if v == false then
            t[k] = ""
        end
    end
    return (string.gsub(s, ".", t))
end

print(transliterate("ala bala",
    { a = "b", l = "m", b = "a", [" "] = false }))
