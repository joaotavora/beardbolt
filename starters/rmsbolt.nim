import strutils

# Nim beardbolt starter file

# Local Variables:
# beardbolt-command: "nim c --opt:size"
# End:



proc isRMS(a: char): uint8 {.exportc.} =
  case a
  of 'R': result = 1
  of 'M': result = 2
  of 'S': result = 3
  else: result = 0

proc main(): void =
  const a: char = cast[char](1 + 1)
  if (isRMS(a) != 0): 
    echo "$#" % $a
