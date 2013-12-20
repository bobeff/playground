--[[
    Write coroutine iterator for all combinations of an array of n elemnets
k-th class in lexicographical order of indices of the array.
]]

function combinations(a, k)
    local function combgen(a, k)
        local comb = {}
        local used = {}

        local function gen(i, j)
            if i == k + 1 then
                coroutine.yield(comb)
            else
                for k = j, #a do
                    if not used[k] then
                        used[k] = true
                        comb[i] = a[k]
                        gen(i + 1, k)
                        used[k] = false
                    end
                end
            end
        end

        gen(1, 1)
    end

    return coroutine.wrap(function() combgen(a, k) end)
end

for c in combinations({'a', 'b', 'c', 'd'}, 2) do
    print(table.unpack(c))
end
