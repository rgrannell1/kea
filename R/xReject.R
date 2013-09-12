
#' xReject
#' 
#' Remove all elements from a collection matching a predicate.
#'
#' @param pred a predicate.
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xReject <- function (pred, coll) {
	# (a -> boolean) -> Collection a -> [a]
	# returns collection[i] such that 
	# pred(collection[i]) is false

	pcall <- sys.call()

	assert(
		is.function(pred) || is.symbol(pred) || 
		(is.character(pred) && length(pred) == 1), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)
	
	pred <- match.fun(pred)
	require_a("unary function", pred)

	if (length(coll) == 0) {
		list()
	} else {
		ind <- vapply(coll, pred, logical(1), USE.NAMES = False)
		as.list( coll[`_`(ind) | !ind ] )			
	}
}
