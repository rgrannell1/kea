
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

#| function: xIsFalse version: 0.1 finished: false

xIsFalse <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection false?

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	unname(vapply(coll, function (x) {
		identical(x, False)
	}, TRUE))
}
