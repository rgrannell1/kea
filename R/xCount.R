
#' Count the number of elements in a collection for which a predicate is true.
#'
#' @param pred a unary function that returns a boolean value.
#' @param coll a list, vector or pairlist.
#'
#' @section Corner Cases:
#'     if \code{coll} is zero is returned.
#'
#' @export

#| function: xCount version: 0.1 finished: false 

xCount <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# return the number of elements for which a predicate is true.

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("listy", coll, pcall)

	pred <- match.fun(pred)
	require_a('unary function', pred)
	
	if (length(coll) == 0) {
		0
	} else {
		ith <- 1
		count <- 0
		while (ith <= length(coll)) {
			
			res <- pred( coll[[ith]] )
			
			if (isTRUE(res)) {
				count <- count + 1
			}
			
			ith <- ith + 1
		}
		count
	}
}
