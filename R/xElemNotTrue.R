
#' xElemNotTrue
#'
#' Is an element of a collection not true?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being non-true.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns logical(0) if \bold{coll} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemNotTrue.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemNotTrue
#' @export

xElemNotTrue <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not true?

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (x) {
			!identical(x, True)
		}, logical(1), USE.NAMES = False)
	}
}

#' @rdname xElemNotTrue
#' @export

xElemNotTrue... <- function (...) {
	xElemNotTrue(list(...))
}
