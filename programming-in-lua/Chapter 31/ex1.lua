local ex1mod = require"ex1mod"

local foo = coroutine.wrap(ex1mod.fooc)
print(foo())

-- Answer:
-- The calling function receives the top nargs values in the
-- coroutine stack.
