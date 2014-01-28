
#' xNotFalse
#'
#' Is an value not sfalse?
#'
#' @param
#'    val an arbitrary value.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xNotFalse.R
#'
#' @section Corner Cases:
#'     xNotFalse returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     True is returned.
#'
#' @family testing_functions
#'
#' @rdname xNotFalse
#' @export

xNotFalse <- function (val) {
	# any -> boolean
	# test if a value is na

	invoking_call <- sys.call()

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	if (length(val) == 0) {
		True
	} else {
		!identical(val, False)
	}
}
