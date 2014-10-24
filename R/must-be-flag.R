
Must_Be_Flag <- function (BOOL, PRED) {
	# this macro expands to check if a value is True, False or Na.

	BOOL <- substitute(BOOL)
	PRED <- substitute(PRED)

	bquote( if (!is.logical( .(BOOL) ) || length( .(BOOL) ) != 1) {

		message <-
			"The predicate function " %+% ddquote( .(PRED) ) %+%
			" produced a non-{True, False, Na} value." %+%
			summate( .(BOOL) )

		throw_exception $ error(sys.call(), message)

	} )
}
