#' Return the index of the last element in \code{collection} for which \code{pred} returns true.
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

#| function: xPositionr version: 0.1 finished: false 

xPositionr <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# returns the last index of collection that matches
	# the predicate.

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll)
	
	pred <- match.fun(pred)
	require_a('unary function', pred, pcall)

	if (length(coll) == 0) {
		integer(0)
	} else {
		ith <- length(coll)

		while (ith >= 1) {

			is_match <- pred( coll[[ith]] )
			stopifnot(is.logical(is_match))

			if (is_match) {
				return (ith)
			}
			ith <- ith - 1
		}	
		integer(0)
	}

}
