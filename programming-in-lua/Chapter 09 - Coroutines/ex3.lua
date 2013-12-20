--[[
    Implement a transfer function in Lua. If you think about resume-yeild
as similar to call-return, transfer would be like a goto. It suspends
the running coroutine and resumes any other coroutine, given as an argument.
]]

function transfer(co)
    coroutine.yield(co)
end

threads = {}

threads[1] = coroutine.create(function()
    while true do
        local move = io.read()
        if move == "south" then
            transfer(threads[3])
        elseif move == "east" then
            transfer(threads[2])
        else
            print("Invalid move.")
        end
    end
end)

threads[2] = coroutine.create(function()
    while true do
        local move = io.read()
        if move == "south" then
            transfer(threads[4])
        elseif move == "west" then
            transfer(threads[1])
        else
            print("Invalid move.")
        end
    end
end)

threads[3] = coroutine.create(function()
    while true do
        local move = io.read()
        if move == "north" then
            transfer(threads[1])
        elseif move == "east" then
            transfer(threads[4])
        else
            print("Invalid move.")
        end
    end
end)

threads[4] = coroutine.create(function()
    print("Congretualtions, you won!")
end)

function dispatch(i)
    local nextCo = threads[i]
    while true do
        local status, to = coroutine.resume(nextCo)
        if not status then
            print(to)
            break
        end
        if coroutine.status(nextCo) == "dead" then
            break
        end
        nextCo = to
    end
end

dispatch(1)
