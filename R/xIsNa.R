
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
#' @examples 
#' @export

xIsNa <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection na?

	pcall <- sys.call()
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	vapply(coll, function (x) {
		identical(x, NA) ||
		identical(x, NA_integer_) ||
		identical(x, NA_real_) ||
		identical(x, NA_character_) ||
		identical(x, NA_complex_)

	}, logical(1), USE.NAMES = False)
}
