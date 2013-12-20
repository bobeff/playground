--[[
    Can you use the os.execute function to change the current directory of
your Lua programs ? Why ?
]]

os.execute("dir")
os.execute("cd ..")
os.execute("dir")

-- Answer:
-- It is not possible, because ANSI C in which Lua is writen do not support
-- concept for directory.
