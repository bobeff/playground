--[[
    Modify the queue implementation so that both indices return to zero when
the queue is empty.
]]

Queue = {}

function Queue.new()
    return { front = 0; back = 0 }
end

function Queue.is_empty(queue)
    return queue.front == queue.back
end

function Queue.push_front(queue, value)
    queue[queue.front] = value
    queue.front = queue.front + 1
end

function Queue.push_back(queue, value)
    queue.back = queue.back - 1
    queue[queue.back] = value
end

function Queue.pop_front(queue)
    queue.front = queue.front - 1
    local ret = queue[queue.front]
    queue[queue.front] = nil
    if  Queue.is_empty(queue) then
        queue.front = 0
        queue.back = 0
    end
    return ret
end

function Queue.pop_back(queue)
    local ret = queue[queue.back]
    queue[queue.back] = nil
    queue.back = queue.back + 1
    if  Queue.is_empty(queue) then
        queue.front = 0
        queue.back = 0
    end
    return ret
end

q = Queue.new()
assert(Queue.is_empty(q))
Queue.push_front(q, 5)
Queue.push_back(q, 13)
Queue.push_back(q, 10)
Queue.push_front(q, 0)
assert(not Queue.is_empty(q))
assert(Queue.pop_back(q) == 10)
assert(Queue.pop_back(q) == 13)
assert(Queue.pop_back(q) == 5)
assert(Queue.pop_front(q) == 0)
assert(Queue.is_empty(q))
assert(q.front == 0 and q.back == 0)
