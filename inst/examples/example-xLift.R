
# 1. Join multiple predicates into one predicate with '%or%'

is_recursive <- is.list %or% is.pairlist






# 2. is a string a palindrome?

palindromic <- "Marge let a moody baby doom a telegram."

# the letters in the string, right to left.
left_to_right <-
	x_(palindromic) $ xToChars() $ xMap(tolower) $ x_Intersect(coll1 = letters)

# the same string, reversed.
right_to_left <- x_(left_to_right) $ x_Reverse()

# using lift. Inner function roughly transforms
# to xIs( ith := left_to_right[ith], ith := right_to_left[ith] )
# then this property is checked for each index.

xAllOf(
	xLift_(xIs,
		xAt(coll = left_to_right),
		xAt(coll = right_to_left)),
	xIndicesOf(left_to_right))
