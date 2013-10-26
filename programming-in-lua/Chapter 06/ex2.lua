--[[
	Write a funciton that receives a polynomial and returns function that,
when called with a value of x, returns the value of polynomial for that x.
]]

function newPoly(coeff)
	return function (x)
		local len = #coeff
		local res = 0
		for i = 1, len do
			res = res + coeff[i] * x ^ (len - i)
		end
		return res
	end
end

f = newPoly{3, 0, 1}
print(f(0))
print(f(5))
print(f(10))
