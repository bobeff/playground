--[[
	Write an iterator uniqueWords that returns all words from a given file
without repetitions.
]]

function uniqueWords(fileName)
	io.input(fileName)
	local line = io.read("*line")
	local pos = 1;
	local words = {}
	return function()
		while line do
			local s, e = string.find(line, "%w+", pos)
			if s then
				pos = e + 1
				local word = string.sub(line, s, e)
				if words[word] == nil then
					words[word] = true
					return word
				end
			else
				pos = 1;
				line = io.read("*line")
			end
		end
		return nil
	end
end

for word in uniqueWords("words.txt") do
	print(word)
end
