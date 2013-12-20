--[[
	Write an iterator that returns all non-empty substrings of a given
string.
]]

function subStrings(s)
	local i = 1
	local j = 1
	local ss = {} --sub strings
	return function()
		while i <= #s do
			while j <= #s do
				local word = string.sub(s, i, j)
				j = j + 1
				if ss[word] == nil then
					ss[word] = true
					return word
				end
			end
			i = i + 1
			j = i
		end
		return nil
	end
end


for s in subStrings("alabala") do
	print(s)
end
