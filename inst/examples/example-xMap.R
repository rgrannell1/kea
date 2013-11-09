
# capitalise the lower-case letters.
LETTERS <- xMap(toupper, letters)

# grab the second value in a collection
# of collections.

xMap(
	xSecond,
	list(
		list("a", "b", "c"),
		list(1, 2, 3))
)

# a mundane example; doubling
# a vector of integers.

xMap(
	n := {
		n + n
	},
	1:5
)