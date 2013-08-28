

#' xPositionr
#'
#' Get the position of the last element for which a predicate returns true.
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

#| function: xPositionr version: 0.1 finished: false 

xPositionr <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# returns the last index of collection that matches
	# the predicate.

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll)
	
	pred <- match.fun(pred)
	require_a('unary function', pred, pcall)

	if (length(coll) == 0) {
		integer(0)
	} else {
		ith <- length(coll)

		while (ith >= 1) {

			is_match <- pred( coll[[ith]] )
			stopifnot(is.logical(is_match))

			if (is_match) {
				return (as.integer(ith))
			}
			ith <- ith - 1
		}	
		integer(0)
	}

}
