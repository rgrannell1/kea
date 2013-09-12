
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

xAny <- function (pred, coll) {
	# (any -> logical) -> Collection any -> boolean
	# is a predicate true for some member of a collection?

	pcall <- sys.call()

	assert(
		is.function(pred) || is.symbol(pred) || 
		(is.character(pred) && length(pred) == 1), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	pred <- match.fun(pred)
	
	assert(
		xArity(pred) %in% c(1, Inf), pcall)
	
	if (length(coll) == 0) {
		logical(0)
	} else {
		is_match <- vapply(coll, pred, logical(1), USE.NAMES = False)

		if ( all(`_`(is_match)) ) {
			False
		} else {
			any(is_match)
		}
	}
}
