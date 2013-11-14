--[[
    Implement a class StackQueue as a subclass of Stack. Beside the inherited
methods, add to this class a method insertbottom, which inserts an element at the
bottom of the stack. (This method allows us to use objects of the class as
queues.)
]]

require "stack"

StackQueue = Stack:new()

function StackQueue:insertbottom(x)
    table.insert(self, x)
end
