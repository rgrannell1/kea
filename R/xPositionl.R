
#' Return the index of the first element in \code{coll} for which \code{predicate} returns true.
#'
#' @param pred a unary function that returns a logical value, or a
#'     symbol or name identifying such a function.
#' @param coll a list, pairlist, or vector.
#'
#' @return a positive whole number which is an index in the indices of \code{coll}.
#'
#' @section Corner Cases:
#' returns \code{integer(0)} if no match is found, or \code{coll} is length-zero.
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

			is_match <- pred( coll[[ith]] )
			stopifnot(is.logical(is_match))
			
			if (is_match) {
				return (ith)
			}
			ith <- ith + 1
		}	
		integer(0)
	}
}
