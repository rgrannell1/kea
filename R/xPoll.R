
#' xPoll
#' 
#' Count the number of times a function returns true when mapped over a collection.
#'
#' @param pred a unary predicate function.
#' @param coll a collection.
#'
#' @return a non-negative whole number.
#'
#' @template glossary
#'
#' @examples 
#' @export

xPoll <- function (pred, coll) {
	# (any -> logical) -> Collection any -> integer
	# return the number of elements for which a predicate is true.

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)

	pred <- match.fun(pred)
	require_a('unary function', pred)
	
	if (length(coll) == 0) {
		0
	} else {
		ith <- 1
		count <- 0
		while (ith <= length(coll)) {
			
			is_match <- pred( coll[[ith]] )
			
			stopifnot(is.logical(is_match))

			if (isTRUE(is_match)) {
				count <- count + 1
			}
			ith <- ith + 1
		}
		count
	}
}
