
Must_Be_Index_Of <- function (INT, COLL) {
	# this macro expands to check if a value is a presumed integer
	# is within a collection's index bounds.

	INT  <- substitute(INT)
	COLL <- substitute(COLL)

	bquote(

		if (.(INT) < 1) {

			message <-
				"The argument matching " %+% ddquote( .(INT) ) %+%
					" was too small to be a valid index.\n" %+%
				"The actual argument was " %+% .(INT)

			throw_exception $ index_error(sys.call(), message)

		} else if (.(INT) > length( .(COLL) ) ) {

			message <-
				"The argument matching " %+% ddquote( .(INT) ) %+%
				" was larger than the largest index of " %+% ddquote(.(COLL)) %+% ".\n"

				"The actual sizes were " %+% .(INT) %+% .(COLL)

			throw_exception $ index_error(sys.call(), message)

		}

	)

}
