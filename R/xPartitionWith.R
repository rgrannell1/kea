
#' xPartitionWith
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
#'	 if \code{coll} is empty the empty list is returned. If all the
#'	 elements return only true/only false, then one of two sublists will be the
#'	 empty list.
#' @template glossary
#'
#' @examples 
#' @export

xPartitionWith <- function (pred, coll) {
	# (any -> logical) -> Collection any -> [[any],[any]]
	# returns two lists; a list for which pred returns 
	# true, and a list for which pred returns false
	
	pcall <- sys.call()
	
	assert(
		is.function(pred) || is.symbol(pred) || 
		(is.character(pred) && length(pred) == 1), pcall)
	
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	pred <- match.fun(pred)
	
	assert(
		xArity(pred) %in% c(1, Inf), pcall)

	if (length(coll) == 0) {
		list()
	} else {
		ind <- vapply(coll, pred, logical(1), USE.NAMES = False)
		true_ind <- !is.na(ind) & ind
				
		list(
			as.list(coll[true_ind]),
			as.list(coll[!true_ind]) )
	}
}
