--[[
	As we have seen, a tail call is a goto in disguise. Using this idea
reimplement the simple maze game from Section 4.4 using tail calls. Each block
should become a new function, and each goto becomes a tail call.
]]

function room1()
	local move = io.read()
	if move == "south" then
		return room3()
	elseif move == "east" then
		return room2()
	else
		print "Invalid move"
		return room1()
	end
end

function room2()
	local move = io.read()
	if move == "south" then
		return room4()
	elseif move == "west" then
		return room1()
	else
		print "Invalid move"
		return room2()
	end
end

function room3()
	local move = io.read()
	if move == "north" then
		return room1()
	elseif move == "east" then
		return room4()
	else
		print "Invalid move"
		return room3()
	end
end

function room4()
	print "Congretulations, you won!"
end

room1()
