--[[
	Add a step parameter to the iterator from the previous exercise.
]]

function fromto(from, to, step)
	from = from - step
	return function()
		if math.abs(from - to) >= 0.0001 then
			from = from + step
			return from
		else
			return nil
		end
	end
end

for i in fromto(0, 1, 0.2) do
	print(i)
end
