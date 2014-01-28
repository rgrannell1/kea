
#' xIsTrue
#'
#' Is an value true?
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
#'    inst/examples/example-xIsTrue.R
#'
#' @section Corner Cases:
#'     xIsTrue returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     False is returned.
#'
#' @family testing_functions
#'
#' @rdname xIsTrue
#' @export

xIsTrue <- function (val) {
	# any -> boolean
	# test if a value is na

	invoking_call <- sys.call()

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	if (length(val) == 0) {
		False
	} else {
		isTRUE(val)
	}
}
