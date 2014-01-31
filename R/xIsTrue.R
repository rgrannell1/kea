
#' xIsTrue
#'
#' Is an value true?
#'
#' @details
#'     \bold{xIsTrue} is primarily meant for use with conditional
#'     statements like if and while. \bold{xIsTrue} always returns a
#'     length-one true or false value; conditional statements throw an error
#'     if they are given a length zero logical vector.
#'
#'     The below example will throw an error; forall of an empty list is
#'     logical zero, which if cannot handle:
#'
#'     \code{mybool <- xForall(xI, list())}
#'
#'     \code{if (mybool) 1 else 2}
#'
#'     The correct way to test for truth in Arrow is \bold{xIsTrue}, which will
#'     return false in this case since logical zero isn't the value true.
#'
#'     \code{if (xIsTrue(mybool)) 1 else 2}
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
