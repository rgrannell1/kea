
# Must_Be_Collection_Of_Lengths_In_Range
#
# colls is known to be a collection of collections.

Must_Be_Collection_Of_Lengths_In_Range <- function (COLLS, LOWER, UPPER) {

	COLLS <- substitute(COLLS)
	LOWER <- substitute(LOWER)
	UPPER <- substitute(UPPER)

	bquote( if ( any(.(LOWER) > length(coll) || length(coll) > .(UPPER)) ) {

		message <-
			"The argument matching " %+% ddquote( .(COLLS) ) %+%
			" must be a collection with lengths in the set " %+%
			"{" %+% .(LOWER) %+% ", ..., " %+% .(UPPER) %+% "}" %+%
			summate( .(COLLS) )

		throw_exception $ error(sys.call(), message)
	})
}
