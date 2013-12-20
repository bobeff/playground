local lproc = require"lproc"

lproc.start([[
    repeat
        local x, y, z = lproc.receive("channel_1")
        if x == "finish" then break end
        print("Number: ", x)
        print("Number name: ", y)
        print("Is even: ", z)
        print()
    until false
]])

lproc.start([[
    local function num_to_str(x)
        if x == 1 then
            return "one"
        elseif x == 2 then
            return "two"
        elseif x == 3 then
            return "three"
        elseif x == 4 then
            return "four"
        elseif x == 5 then
            return "five"
        else
            return "unknown number"
        end
    end

    local channel_name = "channel_1"
    for i = 1, 5 do
        lproc.send(channel_name, i, num_to_str(i), i % 2 == 0)
    end
    lproc.send(channel_name, "finish")
]])

lproc.exit()
