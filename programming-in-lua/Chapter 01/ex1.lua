--[[
	Run factorial example. What happens to your program if you enter a negative
number? Modify the example to avoid this probelm.
]]

function fact(n)
	assert(n >= 0 and n == math.floor(n),
		"Argument must be natural number, but n = " .. n)
	if n == 0 then
		return 1
	else
		return n * fact(n - 1)
	end
end

print("Number: ")
n = io.read("*n");
n = math.abs(n);
n = math.floor(n)
print("Fact(" .. n .. ") is " .. fact(n))
