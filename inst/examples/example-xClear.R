
# 1. HackerRank Functional Programming
#    Reverse a list without using reverse

# Kea has a function for reversing a list, but it is trivial to reverse a list with
# xFold. The function xFold needs to be given a value to return if the input collection
# is length-zero; xClear returns the empty version of a collection.

# To do a very tidy job we use xClear to return the correct length-zero value
# depending on the type of collection given. If an integer vector is given,
# xClear will return integer(0), and so on for other vectors.

reverse <- coll := {
	xFold(
		(left : right) := {
			c(right, left)
		},
		xClear(coll),
		coll
	)
}

reverse(1:4)

# c(4, 3, 2, 1)

reverse(integer(0))

# integer(0)
