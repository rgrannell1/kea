
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
#' @examples 
#' @export

xIsTrue <- function (coll) {
	# Collection a -> Vector boolean
	# test which elements of a collection are true

	pcall <- sys.call()

	assert(
		!missing(coll), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	vapply(coll, function (x) {
		identical(x, True)
	}, logical(1), USE.NAMES = False)
}
