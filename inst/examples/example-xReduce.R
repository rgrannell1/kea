
# 1. is a string a palindrome?

palindromic <- "Marge let a moody baby doom a telegram."

# the letters in the string, right to left.
left_to_right <-
x_(palindromic) $ xToChars() $ xMap(tolower) $ x_Select(xIsIn(coll =letters))

# the same string, reversed.
right_to_left <- x_(left_to_right) $ x_Reverse()

xAllOf(
	ith := {
		# the strings are the same, read from either direction.
		xAsCharacter(left_to_right[ith]) == xAsCharacter(right_to_left[ith])
	},
	seq_along(left_to_right)
)
