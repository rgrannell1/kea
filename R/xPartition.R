
#' xPartition
#' 
#' Divide elements in a collection into two sets based on a predicate function.
#'
#' @param pred a predicate.
#' @param coll a collection.
#'
#' @return Returns two lists; a list of elements in a collection for which a
#' predicate returns true, and a list of elements in a collection for which
#' a predicate returns false or na.
#'
#' @section Corner Cases:
#'	 if \code{coll} is empty, a list of two empty lists is returned. If all the
#'	 elements return only true/only false, then one of two sublists will be the
#'	 empty list.
#' @template glossary
#'
#' @examples 
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
		ind <- vapply(coll, pred, logical(1))
		true_ind <- !is.na(ind) & ind
				
		list(
			as.list(coll[true_ind]),
			as.list(coll[!true_ind]) )
	}
}
