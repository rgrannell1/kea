
#' xElemNotNan
#'
#' Is an element of a collection not NaN?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being non-nan.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{coll is length-zero}.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemNotNan.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemNotNan
#' @export

xElemNotNan <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not false?

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (x) {
			!identical(x, NaN)
		}, logical(1), USE.NAMES = False)
	}
}

#' @rdname xElemNotNan
#' @export

xElemNotNan... <- function (...) {
	xElemNotNan(list(...))
}
