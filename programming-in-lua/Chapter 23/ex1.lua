--[[
    Write a funciton that returns the date/time exactly one month after a
given date/time.
]]

function oneMonthAfter(date)
    time = os.time(date)
    local secondsInMonth = 60 * 60 * 24 * 30
    time = time + secondsInMonth
    return os.date("%c", time)
end

print(oneMonthAfter{year = 2000, month = 1, day = 30})
