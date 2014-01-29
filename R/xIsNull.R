
#' xIsNull
#'
#' Is an value null?
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
#'    inst/examples/example-xIsNull.R
#'
#' @section Corner Cases:
#'     xIsNull returns either true or false, to make it
#'     safe for use with if statements.
#'
#' @family testing_functions
#'
#' @rdname xIsNull
#' @export

xIsNull <- function (val) {
	# any -> boolean
	# test if a value is na

	invoking_call <- sys.call()

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	is.null(val)
}
