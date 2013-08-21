
#' Split a list into two lists
#'
#' @param pred a unary function that returns a logical value, or a 
#'     symbol or name identifying such a function.
#' @param collection a list, pairlist, or vector.
#'
#' @return returns a list containing a subset of the elements in \code{collection}.
#'
#' @section Corner Cases:
#'     Returns the emty list if \code{collection} is length-zero.
#'
#' @export

#| function: xSplitWith version: 0.1 finished: false

xSplitWith <- function (pred, collection) {
	# (any -> boolean) -> Collection any -> [[any] [any]]
	# take every element until pred returns false

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", collection, pcall)

	pred <- match.fun(pred)
	require_a("unary function", pred)

	ith <- 1
	if (length(collection) == 0) {
		list(list(), list())
	} else {
		list(
			xTakeWhile(pred, collection),
			xDropWhile(pred, collection)
		)
	}
}