
# Must_Be_Positive_Index
#
# num is known to be either a whole number or infinite value.
#
#

Must_Be_Positive_Index <- function (NUM, COLL) {

	NUM  <- substitute(NUM)
	COLL <- substitute(COLL)

	bquote( if (is.infinite( .(NUM) ) || .(NUM) < 1) {

		message <-
			"The argument matching " %+% ddquote( .(NUM) ) %+%
			" must be a positive index of the collection matching " %+%
			ddquote( .(COLL) ) %+% ".\n\n" %+%
			"The actual argument was " %+% paste( .(NUM) ) %+%
			"\n"

		throw_kiwi_error(sys.call(), message)

	} else {
		TRUE
	})
}