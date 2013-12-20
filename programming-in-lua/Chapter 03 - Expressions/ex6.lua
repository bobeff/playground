--[[
	COnsider the following expression:
		(x and y and (not z)) or ((not y) and x)
	Are the parantheses necessary? Would you recommend their use
in the expression?
]]

for ix = 0, 1 do
	x = ix == 1 and true or false
	for iy = 0, 1 do
		y = iy == 1 and true or false
		for iz = 0, 1 do
			z = iz == 1 and true or false
			r1 = (x and y and (not z)) or ((not y) and x)
			r2 = x and y and not z or not y and x
			print(r1 == r2)
		end
	end
end
