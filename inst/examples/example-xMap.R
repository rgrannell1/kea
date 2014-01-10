
# 1. a toy example; capitalising the
#    lower-case letters.

LETTERS <- xMap(toupper, letters)

list(
    "A", "B", "C", "D", "E", "F",
    "G", "H", "I", "J", "K", "L",
    "M", "N", "O", "P", "Q", "R",
    "S", "T", "U", "V", "W", "X",
    "Y", "Z")

# 2. grab the second value in a collection
#    of collections.

xMap...(
    xSecond,
    list("a", "b", "c"),
    list(1, 2, 3))
)

list("b", 2)
