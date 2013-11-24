--[[
    Rewrite transliterate funciton from exercise 21.3 for UTF-8 characters.
]]

function transliterate(s, t)
    for k, v in pairs(t) do
        if v == false then
            t[k] = ""
        end
    end
    return (string.gsub(s, ".[\128-\191]*", t))
end

print(transliterate("ala bala",
    { a = "b", l = "m", b = "a", [" "] = false }))
print(transliterate("добър ден",
    { ["д"] = "б", ["o"] = "у", [" "] = false }))
