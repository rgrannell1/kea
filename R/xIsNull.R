
#' xIsNull
#'
#' Is an value null?
#'
#' @param
#'    val an arbitrary value. The value to test for
#'    being null.
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

	insist $ must_not_be_missing(val)

	is.null(val)
}
