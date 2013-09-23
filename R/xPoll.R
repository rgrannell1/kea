
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
#' @examples inst/examples/blank.R
#' @export

xPoll <- function (pred, coll) {
	# (any -> logical) -> Collection any -> integer
	# return the number of elements for which a predicate is true.

	pcall <- sys.call()
	
	assert(
		!missing(pred), pcall)
	assert(
		!missing(coll), pcall)
	
	assert(
		is.function(pred) || is.symbol(pred) || 
		(is.character(pred) && length(pred) == 1), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	pred <- match.fun(pred)
	
	assert(
		xArity(pred) %in% c(1, Inf), pcall)
	
	if (length(coll) == 0) {
		0
	} else {
		count <- 0
		for (ith in seq_along(coll)) {
			
			is_match <- pred( coll[[ith]] )
			
			assert(is.logical(is_match), pcall)

			if (isTRUE(is_match)) {
				count <- count + 1
			}
		}
		count
	}
}
