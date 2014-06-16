
# Be_Whole
#
# test if a single number is an element of the set {-Inf, ... -1, 0. +1, ..., +Inf}
#
# the type of the input is NOT assumemd to be Numeric; another function must test for this.
# fails for NA values, NaN values and infinite values.
#

Must_Be_Whole <- function (NUM) {

	NUM <- substitute(NUM)

	bquote( if (is_na( .(NUM) )) {

		message <-
			"The argument matching " %+% ddquote( .(NUM) ) %+%
			" must not be NA; it must be an element of the set {-Inf, ..., -1, 0, +1, ..., +Inf}"

		throw_kiwi_error(sys.call(), message)

	} else if (is_nan( .(NUM) )) {

		message <-
			"The argument matching " %+% ddquote( .(NUM) ) %+%
			" must not be NaN; it must be an element of the set {-Inf, ..., -1, 0, +1, ..., +Inf}"

		throw_kiwi_error(sys.call(), message)

	} else if (round( .(NUM) ) != .( NUM )) {

		message <-
			"The argument matching " %+% ddquote( .(NUM) ) %+% " must be a round number.\n\n" %+%

			"The actual argument was not round: \n" %+%
			paste(.(NUM)) %+%
			"\n"

		throw_kiwi_error(sys.call(), message)

	} else {
		True
	})
}
