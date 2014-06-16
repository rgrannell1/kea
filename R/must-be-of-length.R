
Must_Be_Of_Length <- function (COLL, LENGTHS) {
	# this macro expands to check that a collection has a certain length.

	COLL    <- substitute(COLL)
	LENGTHS <- substitute(LENGTHS)

	bquote(if (length( .(COLL) ) %!in% .(LENGTHS)) {

		message <-
			"The argument matching " %+% ddquote( .(COLL) ) %+%
			" must have length" %+% paste( .(LENGTHS, collapse = ' or ') ) %+% "." %+%
			summate( .(COLL) )

		throw_kiwi_error(sys.call(), message)
	})
}
