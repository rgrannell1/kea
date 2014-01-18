
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
    xSecondOf,
    list("a", "b", "c"),
    list(1, 2, 3))
)

list("b", 2)



# 3. HackerRank Functional Programming
# Evaluate the first 10 terms of the series of e^x

# The number e^x is given by the series

# 1 + x + x^2/2! + x^3/3! + ...,

# This problem is perfect for xMap and xReduce.
# The moderately astute will notice that

# x = x^1 / 1!, and that 1 = x^0 / 0!,

# meaning that the first two terms aren't any more difficult
# to generate than the rest of the series. The series really is

# x^0/0! + x^1/1! + x^2/2! + x^3/3! + ...,

# which means we can map over the sequence 0, 1, ... and generate
# the corresponding term in the series e^x

power_of_e <- x := {

	x_(0:15)$
	xMap(ith := {
		(x^ith) / factorial(ith)
	})$
	x_Reduce('+')

}

power_of_e(1)
2.718282
