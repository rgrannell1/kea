
#' xIsTrue
#'
#' Is an element of a collection true?
#'
#' @param coll a collection.
#'
#' @return a vector of boolean values.
#'
#'
#'
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xIsTrue <- function (coll) {
	# Collection a -> Vector boolean
	# test which elements of a collection are true

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	vapply(coll, function (x) {
		identical(x, True)
	}, logical(1), USE.NAMES = False)
}

#' @export

xIsTrue... <- function (...) {
	xIsTrue(list(...))
}
