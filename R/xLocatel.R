
#' xLocatel
#'
#' Get the position of the first element for which a predicate returns true.
#' 
#' @param pred a predicate function.
#' @coll a collection.
#'
#' @return an integer.
#'
#' @section Corner Cases: 
#'     returns integer(0) if no match is found.
#' @template glossary
#'
#' @examples 
#' @export

xLocatel <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# returns the first index of collection that matches
	# the predicate pred.

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)
	
	pred <- match.fun(pred)
	require_a('unary function', pred, pcall)

	if (length(coll) == 0) {
		integer(0)
	} else {
		ith <- 1

		while (ith <= length(coll)) {

			is_match <- pred( coll[[ith]] )
			stopifnot(is.logical(is_match))
			
			if (is_match) {
				return (as.integer(ith))
			}
			ith <- ith + 1
		}	
		integer(0)
	}
}

#' @export

xLocate <- xLocatel
