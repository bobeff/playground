--[[
	Write a function that receives an array and prints all combinations of the
elements in the array.
]]

function comb(array)

	local n = #array

	for m = 0, n do

		local used = {}
		local comb = {}

		local function combAux(i, n, m)

			if m == 0 then
				io.write("{ ")
				for i = 1, #comb do
					io.write(array[comb[i]] .. " ")
				end
				io.write("}")
				print()
			end

			for j = i, n do
				if used[j] ~= true then
					comb[#comb + 1] = j
					used[j] = true;
					combAux(j + 1, n, m - 1)
					used[j] = false
					comb[#comb] = nil
				end
			end

		end

		combAux(1, n, m)

	end
end

comb{}
print()
comb{1}
print()
comb{1, 2}
print()
comb{1, 2, 3}
print()
comb{4, 3, 2, 1}
