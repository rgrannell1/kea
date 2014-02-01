
#' xNotNan
#'
#' Is an value not nan?
#'
#' @param
#'    val an arbitrary value.The value to test for
#'    not-nan.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xNotNan.R
#'
#' @section Corner Cases:
#'     xNotNan returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     True is returned.
#'
#' @family testing_functions
#'
#' @rdname xNotNan
#' @export

xNotNan <- function (val) {
	# any -> boolean
	# test if a value is na

	invoking_call <- sys.call()

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	if (length(val) == 0) {
		True
	} else {
		is.nan(val)
	}
}
