--[[
	Assume the following code:
		a = {} ; a.a = a
	What would be the value of a.a.a.a? Is any a in the sequece somehow
different then the others?
	Now add the next line to the previous code:
		a.a.a.a = 3
	What would be the value of a.a.a.a now?
]]

a = {}
a.a = a          -- a["a"] = a
print(a.a.a.a)   -- a["a"]["a"]["a"] == a
a.a.a.a = 3      -- a["a"]["a"]["a"] = 3
--print(a.a.a.a) -- a["a"] == 3["a"]["a"] undefined
--print(a.a.a)   -- a["a"] == 3["a"] undefined
print(a)
print(a.a)
