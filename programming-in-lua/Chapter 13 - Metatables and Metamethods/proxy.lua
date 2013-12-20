--[[
    Exercise 13.3: Complete the implementation of proxies in Section 13.4 with
an __ipairs metamethod.
]]

local index = {}

local mt = {
    __index = function(t, k)
        print("*access to element " .. tostring(k))
        return t[index][k]
    end,

    __newindex = function(t, k, v)
        print("*update of element " .. tostring(k) ..
              " to " .. tostring(v))
        t[index][k] = v
    end,

    __pairs = function(t)
        return function(t, k)
            return next(t[index], k)
        end, t
    end,

    __ipairs = function(t)
        return function(t, i)
            if i < #t[index] then
                return i + 1, t[index][i + 1]
            else
                return nil
            end
        end, t, 0
    end,
}

function track(t)
    local proxy = {}
    proxy[index] = t
    setmetatable(proxy, mt)
    return proxy
end

local t = track{1, 2, 3, 4, 5}
for i, v in ipairs(t) do
    print(string.format("t[%d] = %s", i, tostring(v)))
end

t[3] = t[5]

for k, v in pairs(t) do
    print(string.format(
        type(k) == "number" and "t[%d] = %s" or "t[%q] = %s", k, v))
end
