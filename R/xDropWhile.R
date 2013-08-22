
#' Take every element in a collection from the first time a predicate
#' is false or na until the end of the collection.
#'
#' @param pred a unary function that returns a logical value, or a 
#'     symbol or name identifying such a function.
#' @param coll a list, pairlist, or vector.
#'
#' @return returns a list containing a subset of the elements in \code{coll}.
#'
#' @section Corner Cases:
#' Returns the emty list if \code{coll} is length-zero.
#'
#' @family arrow-filters
#' @export

#| function: xDropWhile version: 0.1 finished: false

xDropWhile <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# take every element from the first element for which
	# pred is false to the end of coll

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)

	pred <- match.fun(pred)
	require_a("unary function", pred)

	ith <- 1
	if (length(coll) == 0) {
		list()
	} else {
		while (ith <= length(coll)) {

			is_match <- pred( coll[[ith]] )

			if (!isTRUE(is_match)) {
				return ( as.list(coll[ith:length(coll)]) )
			} else {
				ith <- ith + 1
			}
		}
		list()	
	}
}
