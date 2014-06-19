
# Must_Be_Existing_Ref
#
# sym is assumed to be a symbol.

Must_Be_Existing_Ref <- function (SYM) {

	SYM <- substitute(SYM)

	bquote( if ( !isTRUE( exists( .(SYM), envir = parent.frame()) ) ) {

		message <-
			"The variable referenced by the symbol " %+% ddquote( .(SYM) ) %+%
			" does not exist."

		throw_kiwi_error(sys.call(), message)

	} )
}
