
# Must_Be_Parametres_Of
#
# strs is known to be a character vector, but it may contain empty values.
#

Must_Be_Parametres_Of <- function (STRS, FN) {

	STRS <- substitute(STRS)
	FN   <- substitute(FN)

	# -- filter out empty strings, assert the rest are parametres of fn.
	bquote( if (any( .(STRS)[nchar(.(STRS)) > 0] %not_in% names(formals( .(FN) )) )) {

		message <-
			"The argument matching " %+% ddquote( .(STRS) ) %+%
			" must be parametres of the function matching " %+% ddquote( .(FN) ) %+% "." %+%
			summate( .(STRS) )

		throw_kea_error(sys.call(), message)
	} )

}
