
# 1. Partitioning

mixed <- xShuffle(c(letters, LETTERS))

# -- is the letter a lower-case letter?
xPartition(
	xFix_(xIsMember, coll = letters),
	mixed)

# shuffling will vary the order.
#
# list(
#     list(
#         "n", "w", "a", "z", "i", "y", "b", "t", "c", "u", "j",
#         "o", "f", "d", "v", "k", "g", "p", "r", "q", "x", "m", "s",
#         "e", "l", "h"),
#     list(
#         "C", "D", "V", "N", "Q", "X", "S", "M",
#         "G", "H", "O", "L", "E", "F", "W", "R", "Y", "T", "I", "K",
#         "A", "B", "J", "P", "U", "Z"))
#
