
#' Partition elements in a collection based on a predicate function.
#'
#' @param predicate a unary function that returns a logical value, or a 
#'	 symbol or name identifying such a function.
#' @param coll a list, pairlist, or vector.
#'
#' @return Returns two lists; a list of elements in a collection for which a
#' predicate returns true, and a list of elements in a collection for which
#' a predicate returns false or na.
#'
#' @section Corner Cases:
#'	 if \code{coll} is empty, a list of two empty lists is returned. If all the
#'	 elements return only true/only false, then one of two sublists will be the
#'	 empty list.
#'
#' @export

#| function: xPartition version: 0.1 finished: false

xPartition <- function (pred, coll) {
	# (any -> logical) -> Collection any -> [[any],[any]]
	# returns two lists; a list for which pred returns 
	# true, and a list for which pred returns false
	
	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)

	pred <- match.fun(pred)
	require_a("unary function", pred)

	if (length(coll) == 0) {
		list(list(), list())
	} else {
		ind <- vapply(coll, pred, True)
		true_ind <- !is.na(ind) & ind
				
		list(
			as.list(coll[true_ind]),
			as.list(coll[!true_ind]) )
	}
}
