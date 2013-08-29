
#' xAny
#' 
#' Return a function that tests if a either of pair of functions are true for its input.
#'
#' @param pred1 a predicate.
#' @param pred2 a predicate.
#'
#' @return a unary predicate.
#'
#' @template glossary
#'
#' @examples 
#' @export

#| function: xAny version: 0.1 finished: false 

xAny <- function (pred, coll) {
	# (any -> logical) -> Collection any -> boolean
	# is a predicate true for some member of a collection?

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("collection", coll, pcall)

	pred <- match.fun(pred)
	require_a('unary function', pred)
	
	if (length(coll) == 0) {
		logical(0)
	} else {
		is_match <- vapply(coll, pred, logical(1))

		if ( all(is.na(is_match)) ) {
			False
		} else {
			any(is_match)
		}
	}
}
