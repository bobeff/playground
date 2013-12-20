--[[
	Write a function that receives an arbitary number of strings and returns
all of them concatenating together.
]]

function concat(...)
	local strings = {...}
	local result = ""
	for i = 1, #strings do
		result = result .. strings[i]
	end
	return result
end

print(concat("ala", " ", "bala", " ", "portokala"))
