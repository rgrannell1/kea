
#
#

Must_Be_File <- function (STR) {

	STR <- substitute(STR)

	bquote(if (!file.exists( .(STR) )) {

		message <-
			"The argument matching " %+% ddquote( .(STR) ) %+%
			" must be a path to an existing file."

		throw_kiwi_error(sys.call(), message)
	})
}
