

#' xLocater
#'
#' Get the position of the last element for which a predicate returns true.
#' 
#' @param pred a predicate function.
#' @param coll a collection.
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
		!missing(pred), pcall,
		exclaim$parameter_missing(pred))

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))
	
	assert(
		is_fn_matchable(pred), pcall,
		exclaim$must_be_matchable(pred))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))
	
	pred <- match.fun(pred)
	
	assert(
		xArity(pred) %in% c(1, Inf), pcall,
		exclaim$must_be_unary(pred))

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
