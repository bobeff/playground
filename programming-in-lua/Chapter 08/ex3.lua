--[[
    Function "stringrep uses binary multiplication algorithm to concatenate
n copies of a given string s. For any fixed n, we can create a specialized
version of "stringrep" by unrolling the loop into a sequence of instructions
r = r .. s and s = s .. s. As an example, for n = 5 the unrolling gives us
the following funciton:
    function stringrep_5(s)
        local r = ""
        r = r .. s
        s = s .. s
        s = s .. s
        r = r .. s
    end

    Write a function that, given n, returns a specialized function "stringrep_n"
Instead of using a closure, your function should build the text of a Lua function
with the proper sequece of instructions (r = r .. s and s = s .. s) and then use
"load" to produce the final function. Compare the performance of a generic
function "stringrep" (or of a closure using it) with your tailor-made function.
]]

function stringrep(s, n)
    assert(n >= 0 and n == math.floor(n),
        "n must be natural number")
    local res = ""
    while n > 0 do
        if n % 2 ~= 0 then
            res = res .. s
            n = n - 1
        else
            s = s .. s
            n = n / 2
        end
    end
    return res
end

function createstringrep(n)
    local f = "return function(s)\n"
    f = f .. "\tlocal r = ''\n"
    while n > 0 do
        if n % 2 ~= 0 then
            f = f .. "\tr = r .. s\n"
            n = n - 1
        else
            f = f .. "\ts = s .. s\n"
            n = n / 2
        end
    end
    f = f .. "\treturn r\n";
    f = f .. "end\n"
    return load(f)()

end

local times = 300000000

local t = os.clock()
stringrep("a", times)
print(os.clock() - t)

f = createstringrep(times)
t = os.clock()
f("a")
print(os.clock() - t)
