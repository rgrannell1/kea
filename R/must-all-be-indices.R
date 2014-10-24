
# Must_Be_Indices
#
# num is known to be either a whole number or infinite value.

Must_All_Be_Indices <- function (NUMS, COLL) {

	NUMS <- substitute(NUMS)
	COLL <- substitute(COLL)

	bquote( if ( any(is.infinite( .(NUMS) )) ) {

		message <-
			"The argument matching " %+% ddquote( .(NUMS) ) %+%
			" must be positive or negative indices of the collection matching " %+% ddquote( .(COLL) ) %+%
			".\n\n" %+%
			"The actual argument contained infinite values." %+%
			summate( .(NUMS) )

		throw_exception $ error(sys.call(), message)

	} else if ( any( abs( .(NUMS) ) > length( .(COLL) )) ) {

		message <-
			"The argument matching " %+% ddquote( .(NUMS) ) %+%
			"must be positive of negative indices of the collection matching " %+% ddquote( .(COLL) ) %+% "." %+%
			summate( .(NUMS) )

		throw_exception $ error(sys.call(), message)

	})

}
