
# 1. Find the smallest number in a list.

x__(1, 2, 3, 4, 5) $ x_MinBy(xI)

# 1

# 2. CE is a string a pangram?
#    does a string contain each letter.

is_pangram <- line := {

	x_(line) $
	xToChars() $ xMap(tolower) $ xUniqueOf() $
	xSelect(
		xIsIn(coll = letters)
	) $
	x_LenOf() == 26
}

is_pangram('A quick brown fox jumps over the lazy dog')

# True

is_pangram('not pangramic thank you mam.')

# False
