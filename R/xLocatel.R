
#' xLocate
#'
#' Get the position of the first element for which a predicate returns true.
#' 
#' @param pred a predicate function.
#' @coll a collection.
#'
#' @return an integer.
#'
#' @section Corner Cases: 
#'     returns integer(0) if no match is found.
#' @template glossary
#'
#' @examples 
#' @export

xLocate <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# returns the first index of collection that matches
	# the predicate pred.

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
		integer(0)
	} else {

		for (ith in seq_along(coll)) {

			is_match <- pred( coll[[ith]] )
			assert(is.logical(is_match), pcall)
			
			if (is_match) {
				return (as.integer(ith))
			}
		}	
		integer(0)
	}
}

#' @export

xLocatel <- xLocate
