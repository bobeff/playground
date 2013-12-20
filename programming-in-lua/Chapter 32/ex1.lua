local memlimit = require"memlimit"

memlimit.setlimit(1024 * 1024)

local t = {}
collectgarbage("stop")
for i = 0, math.huge do
    print(i)
    t[tostring(i)] = i
end
