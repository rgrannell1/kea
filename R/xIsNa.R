
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

#| function: xIsNa version: 0.1 finished: false

xIsNa <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection na?

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	vapply(coll, function (x) {
		identical(x, Na)
	}, logical(1), USE.NAMES = False)
}
