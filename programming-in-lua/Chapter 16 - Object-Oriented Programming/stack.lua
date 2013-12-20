--[[
    Implement a class Stack, with methods push, pop, top and isempty.
]]

Stack = {}

function Stack:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Stack:push(x)
    self[#self + 1] = x
end

function Stack:pop()
    local x = self[#self]
    self[#self] = nil
    return x
end

function Stack:top()
    return self[#self]
end

function Stack:isempty()
    return #self == 0
end
