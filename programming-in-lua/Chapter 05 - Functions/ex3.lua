--[[
	Write a function that receives an arbitary number of values and returns all
of them, except the first one.
]]

function withoutFirst(...)
	local t = {...}
	local res = {}
	for i = 2, #t do
		res[#res + 1] = t[i]
	end
	return table.unpack(res)
end

print(withoutFirst(1, 2, 3))
