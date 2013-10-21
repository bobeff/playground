--[[
	Run the twice example, both by loading the file with the -l option and
with dofile. What way do you prefer ?
]]

function norm(x, y)
	return (x ^ 2 + y ^ 2) ^ 0.5
end

function twice(x)
	return 2 * x
end
