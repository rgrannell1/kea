
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
#' @examples 
#' @export

xIsFalse <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection false?

	pcall <- sys.call()

	assert(
		!missing(coll), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	vapply(coll, function (x) {
		identical(x, False)
	}, logical(1), USE.NAMES = False)
}
