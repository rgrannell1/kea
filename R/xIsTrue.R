
#' xIsTrue
#' 
#' Is an element of a collection true?
#'
#' @param coll a collection.
#'
#' @return a vector of boolean values.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xIsTrue <- function (coll) {
	# Collection a -> Vector boolean
	# test which elements of a collection are true

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	vapply(coll, function (x) {
		identical(x, True)
	}, logical(1), USE.NAMES = False)
}
