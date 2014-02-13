
#' xIsNa
#'
#' Is an value na?
#'
#' @param
#'    val an arbitrary value. The value to test for being na.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xIsNa.R
#'
#' @section Corner Cases:
#'     xIsNa returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     False is returned.
#'
#' @family testing_functions
#'
#' @rdname xIsNa
#' @export

xIsNa <- function (val) {
	# any -> boolean
	# test if a value is na

	invoking_call <- sys.call()

	insist $ must_not_be_missing(val)

	if (length(val) == 0) {
		False
	} else {
		is.na(val)
	}
}
