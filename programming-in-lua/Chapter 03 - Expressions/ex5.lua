--[[
	How can you check whether the value is boolean without using the type
function?
]]

function isBoolean(value)
	return value == true or value == false
end

print(isBoolean(true))
print(isBoolean(false))
print(isBoolean(nil))
print(isBoolean(42))
