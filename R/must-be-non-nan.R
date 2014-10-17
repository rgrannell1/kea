
Must_Be_Non_Nan <- function (NUM) {

	NUM <- substitute(NUM)

	bquote( if (is_nan( .(NUM) )) {

		message <-
			"The argument matching " %+% ddquote(.(NUM)) %+%
			" must not be NaN.\n" %+%
			"The actual argument was " %+% paste(.(NUM)) %+% ".\n"
		throw_kea_error(sys.call(), message)

	} )
}
