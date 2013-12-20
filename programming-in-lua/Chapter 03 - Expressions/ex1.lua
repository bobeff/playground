--[[
	What will the following program print?
]]

for i = -10, 10 do
	print(i, i % 3)
end

-- the formula is a % b == a - math.floor(a / b) * b

for i = -10, 10 do
	print(i, i - math.floor(i / 3) * 3)
end
