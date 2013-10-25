--[[
	Write a function that receives an array and prints all elements in that
array. Consider the pros and cons of using table.unpack in this function.
]]

function printArray1(array)
	for i = 1, #array do
		print(array[i])
	end
end

function printArray2(array)
	print(table.unpack(array))
end

array = {3, 2, 1, 5, 4}
printArray1(array)
printArray2(array)
