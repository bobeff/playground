--[[
	Write an iterator "fromto(n, m)" such that iterates between n and m,
]]

local function nextNumber(to, from)
	return from < to and from + 1 or nil
end

function fromto(from, to)
	return nextNumber, to, from - 1
end

for i in fromto(0, 9) do
	print(i)
end
