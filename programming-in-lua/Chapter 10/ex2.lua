--[[
    Alternative implementation for the eight-queen problem would be to
generate all possible permutations of 1 to 8 and, for each permutation,
to check wether it is valid. Change the program to use this approach.
How does the performance of the new program compare to with the old one ?
]]

function permutations(n)

    local used = {}
    local perm = {}

    local function genperm(i)
        if n + 1 == i then
            coroutine.yield(perm)
        else
            for j = 1, n do
                if not used[j] then
                    used[j] = true
                    perm[i] = j
                    genperm(i + 1)
                    used[j] = false
                end
            end
        end
    end

    local function permgen()
        genperm(1)
    end

    return coroutine.wrap(permgen)
end

function isValid(board)
    for row = 2, #board do
        for r = 1, row - 1 do
            if board[row] == board[r] then return false end
            if board[row] - row == board[r] - r then return false end
            if board[row] + row == board[r] + r then return false end
        end
    end
    return true
end

local boardNumber = 0

function printBoard(board)
    boardNumber = boardNumber + 1
    io.write("Board " .. boardNumber .. "\n")
    for i = 1, #board do
        for j = 1, #board do
            io.write(board[i] == j and "X " or "- ")
        end
        io.write("\n")
    end
    io.write("\n")
end

for p in permutations(8) do
    if isValid(p) then
        printBoard(p)
    end
end

-- disable solution print for real result
print(os.clock())
