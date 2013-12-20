require "stack_queue"

s = StackQueue:new()

for i = 1, 10 do
    s:push(i)
    s:insertbottom(10 - i + 1)
end

for i = 1, 20 do
    print(s:pop())
end
