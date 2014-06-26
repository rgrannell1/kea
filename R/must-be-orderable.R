


Must_Be_Orderable <- function (NUMS) {

	NUMS <- substitute(NUMS)

	# -- is.na also tests for NaN, is more efficient than two checks.
	bquote( if ( any(is.na( .(NUMS) )) ) {

		message <-
			"The argument matching " %+% ddquote( .(NUMS) ) %+%
			" must be a collection of orderable elements, but contained NA or NaN values." %+%
			summate(.(NUMS))

		throw_kiwi_error(sys.call(), message)

	} )
}
