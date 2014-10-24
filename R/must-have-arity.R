
# Must_Have_Arity
#
# This macro only covers some possible problems with arity.
#
# 1, When more arguments are supplied than there are parametres.
#
#

Must_Have_Arity <- function (FN, NUM) {

	FN <- substitute(FN)
	NUM <- substitute(NUM)

	bquote( if (
		!any(params_of( .(FN) ) == '...') &&
		length(params_of( .(FN) )) < .(NUM)) {

		arity <- length(params_of( .(FN) ))

		message <-
			"The argument matching " %+% ddquote( .(FN) ) %+%
			" must be a function that can accept " %+% .(NUM) %+%
			pluralise(" argument", .(NUM)) %+% ". " %+%
			"\nThe actual function accepts " %+% arity %+%
			pluralise(" argument", arity) %+% "."

		throw_exception $ error(sys.call(), message)

	} )

}
