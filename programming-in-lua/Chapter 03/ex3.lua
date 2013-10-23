--[[
	We can represent a polynomial:
	An * x ^ n + An-1 * x ^ n - 1 + ... + A1 * x + A0
as list of its coefficients, such as {A0, A1, ..., An}
	Write a function that receives a polynomial (represent as a table)
and a value x and  returns the polynomial value.
]]

function evaluate(coeff, x)
	local len = #coeff
	local res = 0
	for i = 0, len - 1 do
		res = res + coeff[i + 1] * x ^ i
	end
	return res
end

poly = { 1, 2, 3, 4, 5 }
print(evaluate(poly, 10))
