
Must_Not_Contain_Na <- function (COLL) {

	COLL <- substitute(COLL)

	bquote( if ( any(elem_is_na( .(COLL) )) ) {

		message <-
			"The argument matching " %+% ddquote(.(COLL)) %+%
			" must not contain NA values." %+%
			summate(.(COLL))

		throw_exception $ val_error(sys.call(), message)

	} )
}
