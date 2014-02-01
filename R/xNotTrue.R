
#' xNotTrue
#'
#' Is an value not true?
#'
#' @param
#'    val an arbitrary value. The value to test for
#'    not-truth.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xNotTrue.R
#'
#' @section Corner Cases:
#'     xNotTrue returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     True is returned.
#'
#' @family testing_functions
#'
#' @rdname xNotTrue
#' @export

xNotTrue <- function (val) {
	# any -> boolean
	# test if a value is na

	invoking_call <- sys.call()

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	if (length(val) == 0) {
		True
	} else {
		identical(val, True)
	}
}
