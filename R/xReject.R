
#' Remove all elements from a collection matching a predicate.
#'
#' @param pred a unary function that returns a logical value, or a 
#'     symbol or name identifying such a function.
#' @param collection a list, pairlist, or vector.
#'
#' @return a list containing a subset of elements in \code{collection}.
#'
#' @section Corner Cases:
#' returns the empty list if no match is found, or \code{collection} is length-zero.
#'
#' @family arrow-filters
#' @export

#| function: xReject version: 0.1 finished: false 

xReject <- function (pred, collection) {
	# (a -> boolean) -> Collection a -> [a]
	# returns collection[i] such that 
	# pred(collection[i]) is false

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", collection, pcall)
	
	pred <- match.fun(pred)
	require_a("unary function", pred)

	if (length(collection) == 0) {
		list()
	} else {
		ind <- unlist(lapply(collection, pred))		
		as.list( collection[is.na(ind) | !ind ] )			
	}
}
