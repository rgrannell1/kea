
# Must_All_Be_Whole
#
#
#
#

Must_All_Be_Whole <- function (NUMS) {

	NUMS <- substitute(NUMS)

	bquote( if (any( elem_is_na( .(NUMS) ) )) {

		message <-
			"The argument matching " %+% ddquote( .(NUMS) ) %+% " must not contain NA or NaN values;" %+%
			" it must be an element of the set {-Inf, ..., -1, 0, +1, ..., +Inf}" %+%
			summate( .(NUMS) )

		throw_kiwi_error(sys.call(), message)

	} else if (isTRUE(round( .(NUMS) ) != .(NUMS))) {

		message <-
			"The argument matching " %+% ddquote( .(NUMS) ) %+% " must be a collection of round numbers.\n\n" %+%
			summate(.(NUMS))

		throw_kiwi_error(sys.call(), message)

	} else {
		TRUE
	} )

}
