
#' Take every element in a collection from the start of the collection
#' until the predicate is false or na.
#'
#' @param pred a unary function that returns a logical value, or a 
#'     symbol or name identifying such a function.
#' @param collection a list, pairlist, or vector.
#'
#' @return returns a list containing a subset of the elements in \code{collection}.
#'
#' @section Corner Cases:
#' Returns the emty list if \code{collection} is length-zero.
#'
#' @family arrow-filters
#' @export

#| function: xTakeWhile version: 0.1 finished: false

xTakeWhile <- function (pred, collection) {
	# (any -> boolean) -> Collection any -> [any]
	# take every element until pred returns false

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", collection, pcall)

	pred <- match.fun(pred)
	require_a("unary function", pred)

	ith <- 1
	if (length(collection) == 0) {
		list()
	} else {
		while (ith <= length(collection)) {

			isnt_match <- !isTRUE(pred( collection[[ith]] ))

			if (isnt_match) {
				return ( as.list(head(collection, ith - 1)) )
			}
			ith <- ith + 1
		}
		collection		
	}
}
