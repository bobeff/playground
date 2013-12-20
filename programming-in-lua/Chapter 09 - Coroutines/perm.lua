--[[
    Write a coroutine iterator that generates all permutaions of an array in
lexicographical order of array indexes.
]]

function permutations(a)
    local function permgen(a)
        local perm = {}
        local used = {}

        local function gen(i)
            if i == #a + 1 then
                coroutine.yield(perm)
            else
                for j = 1, #a do
                    if not used[j] then
                        used[j] = true
                        perm[i] = a[j]
                        gen(i + 1)
                        used[j] = false
                    end
                end
            end
        end

        gen(1)
    end

    return coroutine.wrap(function() permgen(a) end)
end

for p in permutations{'a', 'b', 'c', 'd'} do
    print(table.unpack(p))
end
