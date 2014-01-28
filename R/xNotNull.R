
#' xNotNull
#'
#' Is an value not null?
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
#'    inst/examples/example-xNotNull.R
#'
#' @section Corner Cases:
#'     xNotNull returns either true or false, to make it
#'     safe for use with if statements.
#'
#' @family testing_functions
#'
#' @rdname xNotNull
#' @export

xNotNull <- function (val) {
	# any -> boolean
	# test if a value is na

	invoking_call <- sys.call()

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	is.null(val)
}
