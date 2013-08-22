
#' Split a list into two lists; all the elements before the first time a predicate
#' returns false, and all the elements including and after that point.
#'
#' @param pred a unary function that returns a logical value, or a 
#'     symbol or name identifying such a function.
#' @param coll a list, pairlist, or vector.
#'
#' @return returns a list containing a subset of the elements in \code{coll}.
#'
#' @section Corner Cases:
#'     Returns the emty list if \code{coll} is length-zero.
#'
#' @export

#| function: xSplitWith version: 0.1 finished: false

xSplitWith <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [[any] [any]]
	# take every element until pred returns false

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)

	pred <- match.fun(pred)
	require_a("unary function", pred)

	ith <- 1
	if (length(coll) == 0) {
		list(list(), list())
	} else {
		list(
			xTakeWhile(pred, coll),
			xDropWhile(pred, coll)
		)
	}
}