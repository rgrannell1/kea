
#' xElemNotFalse
#'
#' Is an element of a collection not false?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being non-false.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemNotFalse.R
#'
#' @rdname xElemNotFalse
#' @export

xElemNotFalse <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not false?

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (x) {
			!identical(x, False)
		}, logical(1), USE.NAMES = False)
	}
}

#' @rdname xElemNotFalse
#' @export

xElemNotFalse... <- function (...) {
	xElemNotFalse(list(...))
}
