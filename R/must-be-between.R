
# Must_Be_Between
#
# Assumes that lower and upper are whole values, but don't need to be finite.
# num doesn't have to be whole, but most often will be.
#
# Don't do any validation on lower or upper; I set those parametres as literals.

Must_Be_Between <- function (NUM, LOWER, UPPER) {

	NUM <- substitute(NUM)

	bquote(if (any( .(NUM) > .(UPPER) | .(NUM) < .(LOWER) )) {

		message <-
			"The argument matching " %+% ddquote( .(NUM) ) %+%
			" must be in the range {" %+% .(LOWER) %+% "..." %+% .(UPPER) %+% "}.\n" %+%
			"The actual number was " %+% .(NUM) %+% ".\n"

		throw_exception $ value_error(sys.call(), message)

	})
}