--[[
	Write a function integral that receives a function f and returns
approximation of its integral.
]]

function integral(f, x, y, stepsCount)
	assert(x < y, "x must be smaller then y")
	stepsCount = stepsCount or 1000
	local step = (y - x) / stepsCount
	local sum = 0
	for i = x, y, step do
		sum = sum + step * (f(i + step) + f(i)) * 0.5
	end
	return sum
end

print(integral(function (x) return x end, 0, 6))
print(integral(function (x) return x * x - 2 *x + 1 end, 0, 2))
print(integral(math.sin, 0, math.pi))
print(integral(math.cos, 0, math.pi))
