--[[
	How can you embed the following piece of XML as a string in Lua?
Show at least two different ways.
]]

s1 = [=[
<![CDATA[
  Hello world
]]>
]=];

print(s1);

for i = 1, #s1 do
	print(string.format("%02x", string.byte(s1:sub(i, i))))
end

s2 = "\x3c\x21\x5b\x43\x44\x41\x54\x41\x5b\x0a\z
      \x20\x20\x48\x65\x6c\x6c\x6f\x20\x77\x6f\x72\x6c\x64\x0a\z
	  \x5d\x5d\x3e\x0a"

print(s2)
