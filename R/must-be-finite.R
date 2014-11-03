
# Must_Be_Finite
#
# test if a single number is finite.
#
# Assumes the input is numeric, is either length zero or one.

Must_Be_Finite <- function (NUM) {

	NUM <- substitute(NUM)

	bquote( if (length(.(NUM)) != 0 && is.infinite( .(NUM) )) {

		message <- "The argument matching " %+% ddquote( .(NUM) ) %+%
		" must not be infinite.\n\n" %+%
k
		"The actual argument was " %+% paste(.(NUM)) %+% "." %+%
		'\n'

		throw_exception $ value_error(sys.call(), message)

	})
}
