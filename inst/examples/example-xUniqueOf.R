
# 1. Find the smallest unique number in a list.

lowest <-
x_( list(1, 1, 3, 2, 2, 2, 5, 4, 9, 9) ) $
xUniqueOf() $
x_Tap(xAsDouble %then% min)

# 2. CE is a string a pangram?
#    does a string contain each letter.

is_pangram <- line := {

	x_(line) $
	xToChars() $ xMap(tolower) $ xUniqueOf() $
	xSelect(
		xFix_(xIsMember, coll = letters)
	) $
	x_LenOf() == 26
}

is_pangram('A quick brown fox jumps over the lazy dog')

# True

is_pangram('not pangramic thank you mam.')

# False
