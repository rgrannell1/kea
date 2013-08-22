
#' Return the index of the first element in \code{collection} for which \code{predicate} returns true.
#'
#' @param predicate a unary function that returns a logical value, or a
#'     symbol or name identifying such a function.
#' @param collection a list, pairlist, or vector.
#'
#' @return a positive whole number which is an index in the indices of \code{collection}.
#'
#' @section Corner Cases:
#' returns \code{integer(0)} if no match is found, or \code{collection} is length-zero.
#'
#' @export

#| function: xPositionl version: 0.1 finished: false

xPositionl <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# returns the first index of collection that matches
	# the predicate pred.

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll)
	
	pred <- match.fun(pred)
	require_a('unary function', pred, pcall)

	if (length(coll) == 0) {
		integer(0)
	} else {
		ith <- 1

		while (ith <= length(coll)) {

			res <- pred( coll[[ith]] )
			
			if (isTRUE(res)) {
				return (ith)
			}
			ith <- ith + 1
		}	
		integer(0)
	}
}
