
#' xSplitWith
#' 
#' Split a list into two lists; all the elements before the first time a predicate
#' returns false, and all the elements including and after that point.
#'
#' @param pred a predicate.
#' @param coll a collection.
#'
#' @return a list of two lists.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xSplitWith <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [[any] [any]]
	# take every element until pred returns false

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
		list()
	} else {
		coll <- as.list(coll)
		for (ith in seq_along(coll)) {

			is_match <- pred( coll[[ith]] )
			assert(is.logical(is_match), pcall)

			if (!is_match) {
				return ( 
					list(
						head(coll, ith - 1),
						tail(coll, length(coll) - (ith - 1)) ))
			}
		}
		list(coll, list())
	}
}
