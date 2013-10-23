--[[
	Rewrite the state machine of Listing 4.2 without using goto.
]]

local map =
{
	["room1"] =
	{
		["south"] = "room3",
		["east" ] = "room2",
	},
	["room2"] =
	{
		["south"] = "room4",
		["west" ] = "room1",
	},
	["room3"] =
	{
		["north"] = "room1",
		["east" ] = "room4",
	},
}

local room = "room1"
while room ~= "room4" do
	local direction = io.read()
	local nextRoom = map[room][direction]
	if nextRoom then
		room = nextRoom;
	else
		print("Invalud move")
	end
end

print("Congretulations, you won!")
