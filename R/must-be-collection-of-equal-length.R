
Must_Be_Collection_Of_Equal_Length <- function (COLLS) {

	COLLS <- substitute(COLLS)

	bquote({

		all_equal <-
			length( .(COLLS) ) == 0 ||
			all(vapply( .(COLLS), function (coll) {
				length(coll) == length( .(COLLS)[[1]] )
			}, logical(1)))

		if (!all_equal) {

			message <-
				"The argument matching " %+% ddquote( .(COLLS) ) %+%
				" must be a collection of collections with equal lengths." %+%
				summate( .(COLLS) )

			throw_kiwi_error(sys.call(), message)
		}
	})

}
