
# Must_All_Be_Positive_Indices
#
# nums are known to be either whole numbers or infinitely large values.

Must_All_Be_Positive_Indices <- function (NUMS, COLL) {

	NUMS <- substitute(NUMS)
	COLL <- substitute(COLL)

	bquote( if ( any(is.infinite( .(NUMS) )) ) {

		message <-
			"The argument matching " %+% ddquote( .(NUMS) ) %+%
			" must be positive indices of the collection matching " %+% ddquote( .(COLL) ) %+%
			".\n\n" %+%
			"The actual argument contained infinite values." %+%
			summate( .(NUMS) )

		throw_kiwi_error(sys.call(), message)

	} else if (any( .(NUMS) > length( .(COLL) ) | .(NUMS) < 1 )) {

		message <-
			"The argument matching " %+% ddquote( .(NUMS) ) %+%
			"must be positive indices of the collection matching " %+% ddquote( .(COLL) ) %+% "." %+%
			summate( .(NUMS) )

		throw_kiwi_error(sys.call(), message)

	} else {
		TRUE
	})
}
