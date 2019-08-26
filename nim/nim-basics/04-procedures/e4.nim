# Create two procedures tick and tock which echo out the words "tick" and
# "tock". Have a global variable to keep track of how many times they have run,
# and run one from the other until the counter reaches 20. The expected output
# is to get lines with "tick" and "tock" alternating 20 times. (Hint: use
# forward declarations.)

var tickTockCount = 0

proc tock()

proc tick() =
  echo "tick"
  inc tickTockCount
  if tickTockCount < 20:
    tock()

proc tock() =
  echo "tock"
  inc tickTockCount
  if tickTockCount < 20:
    tick()

tick()
