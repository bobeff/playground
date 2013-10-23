--[[
	Suppose that you want to create a table that associates each escape
sequence for strings with its meaning. How could you write constructor for
that table?
]]

escapeSequences =
{
	['\a'] = "bell",
	['\b'] = "back space",
	['\f'] = "form feed",
	['\n'] = "newline",
	['\r'] = "carriage return",
	['\t'] = "horizontal tab",
	['\v'] = "vertical tab",
	['\\'] = "backslash",
	['\"'] = "double quote",
	['\''] = "single quote",
}

for k, v in pairs(escapeSequences) do
	print(v)
end
