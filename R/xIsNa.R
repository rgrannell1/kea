
#' xIsNa
#'
#' Is an element of a collection na?
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

xIsNa <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection na?

	parent_call <- sys.call()

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	vapply(coll, function (x) {
		identical(x, NA) ||
		identical(x, NA_integer_) ||
		identical(x, NA_real_) ||
		identical(x, NA_character_) ||
		identical(x, NA_complex_)

	}, logical(1), USE.NAMES = False)
}
