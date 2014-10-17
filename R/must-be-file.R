
# Must_Be_File
#
# assume str is length one.

Must_Be_File <- function (STR) {

	STR <- substitute(STR)

	bquote( if ( !isTRUE(file.exists( .(STR) )) ) {

		message <-
			"The argument matching " %+% ddquote( .(STR) ) %+%
			" must be a path to an existing file." %+% "\n" %+%
			"The actual path was " %+% dQuote(.(STR)) %+% ".\n"

		throw_kea_error(sys.call(), message)

	} )
}
