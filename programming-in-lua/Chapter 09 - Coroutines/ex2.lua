socket = require("socket")

threads = {}

function receive(connection)
    connection:settimeout(0) -- do not block
    local s, status, partial = connection:receive(4096)
    if status == "timeout" then
        coroutine.yield(connection)
    end
    return s or partial, status
end

function download(host, file)
    local c = assert(socket.connect(host, 80))
    local size = 0
    c:send("GET " .. file .. " HTTP/1.0\r\n\r\n")
    while true do
        local s, status = receive(c)
        size = size + #s
        if status == "closed" then
            break
        end
    end
    c:close()
    print(file, size)
end

function get(host, file)
    local co = coroutine.create(function()
        download(host, file)
    end)
    table.insert(threads, co)
end

function dispatch()
    local i = 1
    local timeout = {}
    while true do
        if threads[i] == nil then
            if i == 1 then
                break
            end
            i = 1
            timeout = {}
        end
        local status, res = coroutine.resume(threads[i])
        if not res then
            table.remove(threads, i)
        else
            i = i + 1
            timeout[#timeout + 1] = res
            if #timeout == #threads then
                socket.select(timeout)
            end
        end
    end
end

host = "www.w3.org"

get(host, "/TR/html401/html40.txt")
get(host, "/TR/2002/REC-xhtml1-20020801/xhtml1.pdf")
get(host, "/TR/REC-html32.html")
get(host, "/TR/2000/REC-DOM-Level2-Core-20001113/DOM2-Core.txt")

dispatch()
