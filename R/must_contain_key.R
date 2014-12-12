
Must_Contain_Key <- function (STR, COLL) {

	STR  <- substitute(STR)
	COLL <- substitute(COLL)

	bquote( if (is.null(names( .(COLL) )) || !any( .(STR) == names( .(COLL) )) ) {

		message <-
			"The argument matching " %+% ddquote( .(COLL) ) %+%
			" did not contain a key matching " %+% ddquote(.( STR )) %+% "."

		throw_exception $ key_error(sys.call(), message)
	})

}
