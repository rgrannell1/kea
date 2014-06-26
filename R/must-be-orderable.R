


Must_Be_Orderable <- function (NUMS) {

	NUMS <- substitute(NUMS)

	bquote( if ( any(elem_is_na( .(NUMS) ) | elem_is_nan( .(NUMS) )) ) {

		message <-
			"The argument matching " %+% ddquote( .(NUMS) ) %+%
			" must be a collection of orderable elements, but contained NA or NaN values." %+%
			summate(.(NUMS))

		throw_kiwi_error(sys.call(), message)

	} )
}
