
Must_Be_Closure <- function (FN) {
	# this macro expands to check if a value is True, False or Na.

	FN <- substitute(FN)

	bquote( if (is.primitive( .(FN) )) {

		message <-
			"The function " %+% ddquote( .(FN) ) %+%
			" must be a non-primitive function." %+%
			summate( .(FN) )

		throw_exception $ value_error(sys.call(), message)

	} )

}
