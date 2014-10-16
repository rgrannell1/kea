
# Must_Be_Collection
#
# test if a value is a list, an atomic typed vector, or a pairlist.
#
# fails for factors.

Must_Be_Collection <- function (COLL) {
	# this macro expands to check if a value is a collection.

	COLL <- substitute(COLL)

	bquote( if (any(class( .(COLL) ) == 'kea')) {

		message <-
			"The argument matching " %+% ddquote( .(COLL) ) %+%
			" must be a list, a pairlist or a typed vector." %+%
			"The argument was of class " %+% dQuote("kea") %+%
			". Did you use the incorrect form of a method? (xMethod vs xMethod_)" %+%
			summate( .(COLL) )

		throw_kea_error(sys.call(), message)

	} else if (!is_atomic( .(COLL) ) && !is_generic( .(COLL) )) {

		message <-
			"The argument matching " %+% ddquote( .(COLL) ) %+%
			" must be a list, a pairlist or a typed vector." %+%
			summate( .(COLL) )

		throw_kea_error(sys.call(), message)

	} )

}
