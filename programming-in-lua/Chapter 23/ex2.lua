--[[
    Write a function that receives a date/time (coded as a number) and
returns the number of seconds passed since the beginning of its respective
day.
]]

function secondsinday(time)
    local date = os.date("*t", time)
    date.hour = 0
    date.min = 0
    date.sec = 0
    return time - os.time(date)
end

print(secondsinday(os.time
    { year = 2013, month = 11, day = 26, hour = 1, min = 2, sec = 5 }))
print(secondsinday(os.time()))
