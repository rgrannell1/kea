
Must_Be_Fn_Matchable <- function (VAL) {
	# this macro expands to check if a value is a function or
	# can be looked up as a function.

	VAL <- substitute(VAL)

	bquote(
		if (is.character( .(VAL) )) {

			if (length(.(VAL)) != 1) {

				message <-
					"The argument matching " %+% ddquote( .(VAL) ) %+%
					" must be a function, or a string or symbol naming a function.\n\n" %+%
					"The actual input was a non-length-one character vector." %+%
					summate(.(VAL))

				throw_exception $ error(sys.call(), message)

			}

		} else if (!is.function(.(VAL)) && !is.name(.(VAL))) {

			message <-
				"The argument matching " %+% ddquote( .(VAL) ) %+%
				" must be a function, or a string or symbol naming a function."

			if (any(class( .(VAL) ) == 'kea')) {

				message <- message %+%
					"The argument was of class " %+% dQuote("kea") %+%
					". Did you use the wrong form of kea method (xMethod vs xMethod_)?" %+%
					summate( .(VAL) )

			} else {
				message <- message %+% summate( .(VAL) )
			}

			throw_exception $ type_error(sys.call(), message)

		}
	)

}
