
#' Include all elements from a collection matching a predicate.
#'
#' @param p a unary function that returns a logical value, or a 
#'	 symbol or name identifying such a function.
#' @param coll a list, pairlist, or vector.
#'
#' @return a list containing a subset of elements in \code{coll}.
#'
#' @section Corner Cases:
#'	 returns the empty list if no match is found, or \code{coll} is length-zero.
#'
#' @family arrow-filters
#' @export

#| function: xSelect version: 0.1 finished: false 

xSelect <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# returns coll[i] such that 
	# pred(coll[i]) is true

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)

	pred <- match.fun(pred)
	require_a("unary function", pred)

	if (length(coll) == 0) {
		list()
	} else {
		ind <- vapply(coll, pred, True)
		as.list( coll[ !is.na(ind) & ind ] )
	}
}
