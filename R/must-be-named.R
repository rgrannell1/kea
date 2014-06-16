
# Must_Be_Named
#
#
#
#

Must_Be_Named <- function (COLL) {

	COLL <- substitute(COLL)

	bquote( if ( is.null(names( .(COLL) )) ) {

			message <-
				"The argument matching " %+% ddquote( .(COLL) ) %+%
				" must be named." %+%
				summate( .(COLL) )

			throw_kiwi_error(sys.call(), message)
		}
	)
}