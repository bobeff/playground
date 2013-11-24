--[[
    The patterns "%D" and "[^%d]" are equivalent. What about the patterns
"[^%d%u]" and "[%D%U]" ?
]]

-- Answer:
-- The patterns are not equivalent. Counter example below.

print(string.match("A1", "[^%d%u]"))
print(string.match("1A", "[^%d%u]"))
print(string.match("A1", "[%D%U]"))
print(string.match("1A", "[%D%U]"))
