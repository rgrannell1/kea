
#' xSelect
#' 
#' Include all elements from a collection matching a predicate.
#'
#' @param pred a predicate.
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}, or no match is found.
#' @template glossary
#'
#' @examples 
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
		ind <- vapply(coll, pred, logical(1))
		as.list( coll[ !`_`(ind) & ind ] )
	}
}
