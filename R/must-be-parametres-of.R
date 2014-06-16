
# Must_Be_Parametres_Of
#
# strs is known to be a character vector.
#
#

Must_Be_Parametres_Of <- function (STRS, FN) {

	STRS <- substitute(STRS)
	FN   <- substitute(FN)

	bquote(if (any( .(STRS) %!in% names(formals( .(FN) )) )) {

		message <-
			"The argument matching " %+% ddquote( .(STRS) ) %+%
			" must be parametres of the function matching " %+% ddquote( .(FN) ) %+% "." %+%
			summate( .(STRS) )

		throw_kiwi_error(sys.call(), message)
	})
}
