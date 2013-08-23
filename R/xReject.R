
#' Remove all elements from a collection matching a predicate.
#'
#' @param pred a unary function that returns a logical value, or a 
#'     symbol or name identifying such a function.
#' @param coll a list, pairlist, or vector.
#'
#' @return a list containing a subset of elements in \code{coll}.
#'
#' @section Corner Cases:
#' returns the empty list if no match is found, or \code{coll} is length-zero.
#'
#' @export

#| function: xReject version: 0.1 finished: false 

xReject <- function (pred, coll) {
	# (a -> boolean) -> Collection a -> [a]
	# returns collection[i] such that 
	# pred(collection[i]) is false

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)
	
	pred <- match.fun(pred)
	require_a("unary function", pred)

	if (length(coll) == 0) {
		list()
	} else {
		ind <- vapply(coll, pred, True)
		as.list( coll[is.na(ind) | !ind ] )			
	}
}
