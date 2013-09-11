
#' xSplitWith
#' 
#' Split a list into two lists; all the elements before the first time a predicate
#' returns false, and all the elements including and after that point.
#'
#' @param pred a predicate.
#' @param coll a collection.
#'
#' @return a list of two lists.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xSplitWith <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [[any] [any]]
	# take every element until pred returns false

	pcall <- sys.call()
	require_a(traits$functionable, pred, pcall)
	require_a(traits$collection, coll, pcall)

	pred <- match.fun(pred)
	require_a("unary function", pred)

	ith <- 1
	if (length(coll) == 0) {
		list()
	} else {
		ith <- 1
		coll <- as.list(coll)
		while (ith <= length(coll)) {

			is_match <- pred( coll[[ith]] )
			stopifnot(is.logical(is_match))

			if (!is_match) {
				return ( 
					list(
						head(coll, ith - 1),
						tail(coll, length(coll) - (ith - 1)) 
				))
			}
			ith <- ith + 1
		}
		list(coll, list())
	}
}
