

Must_Be_Non_Na <- function (VAL) {

	VAL <- substitute(VAL)

	bquote( if (is_na( .(VAL) )) {

		message <-
			"The argument matching " %+% ddquote(.(VAL)) %+%
			" must not be NA.\n\n" %+%
			"The actual argument was NA of type " %+% dQuote(typeof(.(VAL))) %+% ".\n"

		throw_kiwi_error(sys.call(), message)

	} else {
		TRUE
	} )
}
