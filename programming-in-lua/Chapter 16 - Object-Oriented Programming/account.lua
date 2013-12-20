--[[
    Another way to provide privacy for objects is to implement them using
proxies. Each object is represented by an empty proxy table. An internal table
maps proxies to tables that carry the object state. This internal table is not
accessible from the outside, but methods use it to translate their self
parameters to the real tables where thay operate. Iplement the Accont example
using this approach and discuss its pros and cons.
]]

local instances = {}
setmetatable(instances, { __mode = "k" })

Account = {}

function Account:new(balance)
    local object = { balance = balance }
    self.__index = self
    local proxy = {}
    setmetatable(proxy, self)
    instances[proxy] = object
    return proxy
end

function Account:deposit(amount)
    local object = instances[self]
    object.balance = object.balance + amount
end

function Account:withdraw(amount)
    self = instances[self]
    if amount > self.balance then
        error("Insufficient funds", 2)
    end
    self.balance = self.balance - amount
end

function Account:getBalance()
    return instances[self].balance
end

-- test code

local a = Account:new(1000)
print(a:getBalance())
a:withdraw(100)
print(a:getBalance())
local b = Account:new(50)
print(b:getBalance())
a:withdraw(60)
print(a:getBalance())

for k, v in pairs(instances) do
    print(k, v)
end

a = nil
collectgarbage()

for k, v in pairs(instances) do
    print(k, v)
end

b:withdraw(60)
