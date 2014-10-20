
Must_Be_Collection_Of_Equal_Length <- function (COLLS) {

	COLLS <- substitute(COLLS)

	bquote({

		# all(c()) is TRUE.

		if ( !all(vapply( .(COLLS), length, integer(1)) == length(  .(COLLS)[[1]] )) ) {

			message <-
				"The argument matching " %+% ddquote( .(COLLS) ) %+%
				" must be a collection of collections with equal lengths." %+%
				summate( .(COLLS) )

			throw_kea_error(sys.call(), message)
		}
	})

}
