--[[
	The number 12.7 is equal to fraction 127/10, where the denominator is a
power of ten. Can you express it as a common fraction where denominator is a
power ot two? What about the number 5.5?
]]

--[[ Answer:
	12.7 can not be represented as common fraction with denominator which is
power of 2:
		12.7 / 2 ^ 0
		25.4 / 2 ^ 2
		50.8 / 2 ^ 2
	   101.6 / 2 ^ 3
	   203.2 / 2 ^ 4
	   406.4 / 2 ^ 5

	But 5.5 can:
		5.5 / 2 ^ 0
		11  / 2 ^ 1
]]

print(0xbp-1) -- (11 * 16 ^ 0) * (2 ^ -1)
