
# Must_Be_Named

# TODO this is a very weak macro; a better test could be defined.

Must_Be_Named <- function (COLL) {

	COLL <- substitute(COLL)

	bquote( if ( is.null(names( .(COLL) )) ) {

			message <-
				"The argument matching " %+% ddquote( .(COLL) ) %+%
				" must be named." %+%
				summate( .(COLL) )

			throw_kea_error(sys.call(), message)
		}
	)
}
