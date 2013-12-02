--[[
    Exercise 24.2:
        Adapt funciton getvarvalue to work with different coroutines.

    Exercise 24.3:
         Write a funciton setvarvalue similar to getvarvalue.

    Exercise 24.4:
    Write a modification of getvarvalue, called getallvars, that returns table with
all variables that are visible at the calling funciton. (The returned table should
not include environment variables; instead, it should inherit them from the original
environment.)

    Exercise 24.5:
    Write an improved version of debug.debug that runs the given commands
as if thay were in the lexical scope of the calling function. (Hint: run
the commands in an emoty environment and use the __index metamethod attached
to function getvarvalue to do all accesses to variables.)

    Exercise 24.6:
    Improve the previous exercise to handle updates, too.

    Exercise 24.8:
    Write a library for breakpoints. It should offer at least two functions:
        setbreakpoints(function, line) -> returns handle
        removebreakpoint(handle)
    A breakpoint is specified by a function and a line inside that function.
When the program hits a breakpoint, the library should call debug.debug.
(Hint: for a basic implementation, use a line hook that checks whether it is
in a breakpoin; to improve performancem use a call hook to trace the program
execution and only turn the line hook when the program is running the target
function.)
--]]

------------------------------------------------------------------------------
------------------------- Exercise 24.2 solution -----------------------------
------------------------------------------------------------------------------

local function getvarvalue(co, name, level)
    if type(co) ~= "thread" then
        level = name
        name  = co
        co    = nil
    end

    local value
    local found = false

    level = (level or 1) + 1

    -- try local variables
    for i = 1, math.huge do
        local n, v
        if co then
            n, v = debug.getlocal(co, level, i)
        else
            n, v = debug.getlocal(level, i)
        end
        if not n then break end
        if n == name then
            value = v
            found = true
        end
    end
    if found then return value end

    -- try non local variables
    local func
    if co then
        func = debug.getinfo(co, level, "f").func
    else
        func = debug.getinfo(level, "f").func
    end
    for i = 1, math.huge do
        local n, v = debug.getupvalue(func, i)
        if not n then break end
        if n == name then return v end
    end

    -- can not find environment for debugged function
    -- return global environment
    if name == "_ENV" then
        return _ENV
    end

    -- not found; get value from the environment
    local env
    if co then
        env = getvarvalue(co, "_ENV", level)
    else
        env = getvarvalue("_ENV", level)
    end
    return env[name]
end

------------------------------------------------------------------------------
------------------------- Exercise 24.3 solution -----------------------------
------------------------------------------------------------------------------

local function setvarvalue(co, name, level, value)
    if type(co) ~= "thread" then
        value = level
        level = name
        name  = co
        co    = nil
    end

    level = (level or 1) + 1

    -- try local variables
    local index
    for i = 1, math.huge do
        local n, v
        if co then
            n, v = debug.getlocal(co, level, i)
        else
            n, v = debug.getlocal(level, i)
        end
        if not n then break end
        if n == name then index = i end
    end
    if index then
        if co then
            debug.setlocal(co, level, index, value)
        else
            debug.setlocal(level, index, value)
        end
        return
    end

    -- try non local variables
    local func
    if co then
        func = debug.getinfo(co, level, "f").func
    else
        func = debug.getinfo(level, "f").func
    end
    for i = 1, math.huge do
        local n, v = debug.getupvalue(func, i)
        if not n then break end
        if n == name then
            debug.setupvalue(func, i, value)
            return
        end
    end

    -- can not find environment for debugged function
    -- return global environment
    if name == "_ENV" then
        return _ENV
    end

    -- not found; set value to the environment
    local env
    if co then
        env = getvarvalue(co, "_ENV", level)
    else
        env = getvarvalue("_ENV", level)
    end
    env[name] = value
end

------------------------------------------------------------------------------
------------------------- Exercise 24.4 solution -----------------------------
------------------------------------------------------------------------------

local function getallvars(co, level)
    if type(co) ~= "thread" then
        level = co
        co    = nil
    end

    local vars = {}

    level = (level or 1) + 1

    -- try local variables
    for i = 1, math.huge do
        local n, v
        if co then
            n, v = debug.getlocal(co, level, i)
        else
            n, v = debug.getlocal(level, i)
        end
        if not n then break end
        vars[n] = v
    end

    -- try non local variables
    local func
    if co then
        func = debug.getinfo(co, level, "f").func
    else
        func = debug.getinfo(level, "f").func
    end
    for i = 1, math.huge do
        local n, v = debug.getupvalue(func, i)
        if not n then break end
        vars[n] = v
    end

    setmetatable(vars, { __index = _ENV })
    return vars
end

------------------------------------------------------------------------------
-------------------- Exercises 24.5 and 24.6 solutions -----------------------
------------------------------------------------------------------------------

local function _debug(isbreakpoint)
    local env = {}
    local level = isbreakpoint and 6 or 5

    setmetatable(env, {
        __index = function(t, k)
            local value = getvarvalue(k, level)
            return value
        end,
        __newindex = function(t, k, v)
            setvarvalue(k, level, v)
        end,
    })

    local prompt_level = isbreakpoint and 3 or 2
    local info = debug.getinfo(prompt_level, "Sln")
    local prompt = string.format("[%s]:%d (%s)> ",
        info.short_src, info.currentline, info.name)

    while true do
        io.write(prompt)
        local line = io.read("*l")
        if line == "cont" then break end
        local f, err = load(line, line, "t", env)
        if f then
            local status, err = pcall(f)
            if not status then print(err) end
        else
            print(err)
        end
    end
end

------------------------------------------------------------------------------
------------------------- Exercise 24.8 solution -----------------------------
------------------------------------------------------------------------------

-- I will use only line hook and breakpoint will be set only with line number,
-- to be possible anonymous functions debugging.

local breakpoints = {}
local handles = {}

local function onnextline(_, line)
    if not breakpoints[line] then return end
    _debug(true)
end

local function setbreakpoint(line)
    debug.sethook(onnextline, "l")
    breakpoints[line] = true
    local handle = {}
    handles[handle] = line
    return handle
end

local function removebreakpoint(handle)
    breakpoints[handles[handle]] = nil
    handles[handle] = nil
end

return
{
    getvarvalue      = getvarvalue,
    setvarvalue      = setvarvalue,
    getallvars       = getallvars,
    _debug           = _debug,
    setbreakpoint    = setbreakpoint,
    removebreakpoint = removebreakpoint,
}
