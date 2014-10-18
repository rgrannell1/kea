
Must_Be_Of_Length <- function (COLL, LENGTHS) {
	# this macro expands to check that a collection has a certain length.

	COLL    <- substitute(COLL)
	LENGTHS <- substitute(LENGTHS)

	bquote( if ( any(length( .(COLL) ) != .(LENGTHS)) ) {

		message <-
			"The argument matching " %+% ddquote( .(COLL) ) %+%
			" must have length in the set {" %+% toString( .(LENGTHS) ) %+% "}.\n" %+%
			"The actual length was " %+% length(.(COLL)) %+% "." %+%
			summate( .(COLL) )

		throw_kea_error(sys.call(), message)

	} )
}

