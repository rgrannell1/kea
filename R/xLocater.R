

#' xLocater
#'
#' Get the position of the last element for which a predicate returns true.
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
#' @examples inst/examples/blank.R
#' @export

xLocater <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# returns the last index of collection that matches
	# the predicate.

	pcall <- sys.call()
	
	assert(
		!missing(pred), pcall)
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))
	
	assert(
		is.function(pred) || is.symbol(pred) || 
		(is.character(pred) && length(pred) == 1), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall,
		exclaim$must_be_collection(coll))
	
	pred <- match.fun(pred)
	
	assert(
		xArity(pred) %in% c(1, Inf), pcall)

	if (length(coll) == 0) {
		integer(0)
	} else {

		for (ith in length(coll):1) {
			is_match <- pred( coll[[ith]] )
			
			assert(is.logical(is_match), pcall)

			if (is_match) {
				return (as.integer(ith))
			}
		}
		integer(0)
	}
}
