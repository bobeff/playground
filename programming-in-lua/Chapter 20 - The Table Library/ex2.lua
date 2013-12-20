--[[
    A problem with table sort is that sort is not stable, that is, elements
that comparison function considers equal may not keep their original order
in the array after the sort. How can you do a stable sort in Lua ?
]]

-- The merge sort algorithm is stable.

function stablesort(array, comp)

    comp = comp or function(x, y) return x < y end

    local function merge(l, m, r)

        local auxArray = {}

        local k = l
        local i = l
        local j = m + 1

        while i <= m and j <= r do
            if comp(array[i], array[j]) then
                auxArray[k] = array[i]
                i = i + 1
            else
                auxArray[k] = array[j]
                j = j + 1
            end
            k = k + 1
        end

        for x = i, m do
            auxArray[k] = array[x]
            k = k + 1
        end

        for x = j, r do
            auxArray[k] = array[x]
            k = k + 1
        end

        for x = l, r do
            array[x] = auxArray[x]
        end
    end

    local function mergesort(left, right)
        local middle = math.floor((left + right) /  2)
        if right - left > 1 then
            mergesort(left, middle)
            mergesort(middle + 1, right)
        end
        merge(left, middle, right)
    end

    mergesort(1, #array)
end

local function printarray(array)
    for i = 1, #array do
        io.write(array[i], " ")
    end
    io.write("\n")
end

local array = {4, 1, 5, 7, 2, 9, 8, 0, 3, 6}
stablesort(array)
printarray(array)
stablesort(array, function (x, y) return x > y end)
printarray(array)
