
# Must_Be_Equal_Length_To

Must_Be_Equal_Length_To <- function (COLL1, COLL2) {

	COLL1 <- substitute(COLL1)
	COLL2 <- substitute(COLL2)

	bquote(if (length( .(COLL1) ) != length( .(COLL2) )) {

		message <-
			"The argument matching " %+% ddquote( .(COLL1) ) %+%
			" must be equal in length to the argument matching " %+% ddquote( .(COLL2) ) %+% "." %+%
			"The actual lengths were " %+% toString(c(length( .(COLL1) ), length( .(COLL2) )))
			summate( .(COLL1) )

		throw_kea_error(sys.call(), message)
	})
}

