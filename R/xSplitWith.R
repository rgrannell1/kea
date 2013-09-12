
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
	
	assert(
		is.function(pred) || is.symbol(pred) || 
		(is.character(pred) && length(pred) == 1), pcall)
	
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	pred <- match.fun(pred)
	assert(
		xArity(pred) %in% c(1, Inf), pcall)

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
