
#' xTakeWhile
#' 
#' Take every element in a collection from the start until a predicate returns false.
#'
#' @param pred a predicate.
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero or the first element of 
#'     \code{coll} returns false for the predicate. Na values are considered false.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R 
#' @export

xTakeWhile <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# take every element until pred returns false

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
		list()
	} else {
		for (ith in seq_along(coll)) {

			is_match <- pred( coll[[ith]] )
			assert(is.logical(is_match), pcall)

			if (!isTRUE(is_match)) {
				return ( as.list(head(coll, ith - 1)) )
			}
		}
		coll		
	}
}
