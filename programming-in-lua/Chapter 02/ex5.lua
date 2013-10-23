--[[
	Suppose you need to foramt a long sequence of arbitary bytes  as a string
literal in Lua. How wolud you do? Consider issues like readability, maximum
line length, and performance.
--]]

s = "\z
\234\023\123\213\078\012\067\153\023\234\023\123\213\078\012\067\153\023\187\z
\234\023\123\213\078\012\067\153\023\234\023\123\213\078\012\067\153\023\187\z
\234\023\123\213\078\012\067\153\023\234\023\123\213\078\012\067\153\023\187\z
\234\023\123\213\078\012\067\153\023\234\023\123\213\078\012\067\153\023\187\z
\234\023\123\213\078\012\067\153\023\234\023\123\213\078\012\067\153\023\187\z
\234\023\123\213\078\012\067\153\023\234\023\123\213\078\012\067\153\023\187"

-- and so on
