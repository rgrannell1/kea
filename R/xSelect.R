
#' xSelect
#' 
#' Include all elements from a collection matching a predicate.
#'
#' @param pred a predicate.
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero, or no match is found.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R 
#' @export

xSelect <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# returns coll[i] such that 
	# pred(coll[i]) is true

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
		ind <- vapply(coll, pred, logical(1), USE.NAMES = False)
		as.list( coll[ !is.na(ind) & ind ] )
	}
}
