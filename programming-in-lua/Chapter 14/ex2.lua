--[[
    Explain in detail what happens in the following program and what its output
is.
]]

local foo
do
    local _ENV = _ENV
    function foo() print(X) end
end

X = 13
_ENV = nil
foo()
X = 0
