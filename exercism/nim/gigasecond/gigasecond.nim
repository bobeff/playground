import std/[times]

proc addGigasecond*(moment: DateTime): DateTime =
  moment + initDuration(0, 0, 0, 1_000_000_000)
