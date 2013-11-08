
#' xIsFalse
#'
#' Is an element of a collection false?
#'
#' @param coll a collection.
#'
#' @return a vector of boolean values.
#'
#' @template glossary
#'
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xIsFalse <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection false?

	parent_call <- sys.call()

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	vapply(coll, function (x) {
		identical(x, False)
	}, logical(1), USE.NAMES = False)
}
