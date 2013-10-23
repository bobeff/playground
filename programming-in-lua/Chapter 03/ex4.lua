--[[
	Can you write the function from the previus exercise so that it uses at
most n additions and n multiplications (and no exponentiations)?
]]

function evaluate(coeff, x)
	local len = #coeff
	local res = 0
	for i = len, 1, -1 do
		res = res * x
		res = res + coeff[i]
	end
	return res
end

poly = { 1, 2, 3, 4, 5 }
print(evaluate(poly, 10))
