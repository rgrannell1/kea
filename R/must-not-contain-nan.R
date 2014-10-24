
Must_Not_Contain_Nan <- function (COLL) {

	COLL <- substitute(COLL)

	bquote( if ( any(elem_is_nan( .(COLL) )) ) {

		message <-
			"The argument matching " %+% ddquote(.(COLL)) %+%
			" must not contain NA values." %+%
			summate(.(COLL))

		throw_exception $ error(sys.call(), message)

	} )
}
