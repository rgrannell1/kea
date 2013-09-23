
#' xDropWhile
#' 
#' Take every element in a collection from the first time a predicate
#' is false or na until the end of the collection.
#'
#' @param pred a unary predicate.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'	 Returns the emty list if \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R 
#' @export

xDropWhile <- function (pred, coll) {
	# (any -> logical) -> Collection any -> [any]
	# take every element from the first element for which
	# pred is false to the end of coll

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
		ith <- 1
		for (ith in seq_along(coll)) {
			
			is_match <- pred( coll[[ith]] )
			assert(is.logical(is_match), pcall)

			if (!isTRUE(is_match)) {
				return (as.list( tail(coll, length(coll) - (ith - 1)) ))
			}
		}
		list()	
	}
}
