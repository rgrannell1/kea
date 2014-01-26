
palindromic <- "Marge let a moody baby doom a telegram."

left_to_right <-
x_(palindromic) $
xToChars() $
xMap(tolower) $
x_Select(
	function (char) xIsMember(char, letters)
)

right_to_left <- x_(left_to_right) $ x_Reverse()

xForall...(
	ith := {

		xAsCharacter(left_to_right[ith]) == xAsCharacter(right_to_left[ith])
	},
	seq_along(left_to_right)
)
