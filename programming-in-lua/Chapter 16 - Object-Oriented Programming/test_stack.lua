require "stack"

local s = Stack:new()
for i = 1, 10 do
    s:push(i)
end

for i = 1, 9 do
    print(s:pop())
end

print(s:top())
s:pop()
assert(s:isempty{})
