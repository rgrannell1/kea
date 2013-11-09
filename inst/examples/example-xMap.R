
# capitalise the lower-case letters.

LETTERS <- xMap(toupper, letters)

list(
	"A", "B", "C", "D", "E", "F",
	"G", "H", "I", "J", "K", "L",
	"M", "N", "O", "P", "Q", "R",
	"S", "T", "U", "V", "W", "X",
	"Y", "Z")

# grab the second value in a collection
# of collections.

xMap(
	xSecond,
	list(
		list("a", "b", "c"),
		list(1, 2, 3))
)

list("b", 2)

# a mundane example; doubling
# a vector of integers.

xMap(n := {n + n}, 1:5)

list(2L, 4L, 6L, 8L, 10L)
