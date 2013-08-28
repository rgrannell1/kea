
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

#| function: xIsTrue version: 0.1 finished: true

xIsTrue <- function (coll) {
	# Collection a -> Vector boolean
	# test which elements of a collection are true

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	unname(vapply(coll, function (x) {
		identical(x, True)
	}, True))
}
