
Must_Be_Matchable <- function (SYM) {
	# this macro expands to test if a value is a symbol.

	SYM <- substitute(SYM)

	bquote( if (is.character( .(SYM) ) ) {

		if (length( .(SYM) ) != 1) {
			message <-
				"The argument matching " %+% ddquote( "sym" ) %+%
				" must be a symbol or a string that can be used as a variable name." %+%
				summate( .(SYM) )

			throw_kea_error(sys.call(), message)

		} else if (nchar( .(SYM) ) == 0) {
			message <-
				"The argument matching " %+% ddquote( "sym" ) %+%
				" must be a symbol or a string that can be used as a variable name.\n\n" %+%
				"The actual argument was the empty string.\n"

			throw_kea_error(sys.call(), message)
		}

		TRUE

	} else if (!is.name( .(SYM) )) {

		message <-
			"The argument matching " %+% ddquote( "sym" ) %+%
			" must be a symbol or a string that can be used as a variable name." %+%
			summate( .(SYM) )

		throw_kea_error(sys.call(), message)

	} )

}
