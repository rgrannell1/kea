
#' xMapWhen
#'
#' Selectively apply a function to elements in a collection.
#' 
#' @param pred a predicate function.
#' @param fn a unary function.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xMapWhen <- function (pred, fn, coll) {
	# (any -> boolean) -> (any -> any) -> Collection any -> [any]
	# apply the function pred to collection, and apply f to
	# the elements for which pred returned true.

	pcall <- sys.call()

	assert(
		!missing(pred), pcall)
	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is.function(pred) || is.symbol(pred) || 
		(is.character(pred) && length(pred) == 1), pcall)

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall, 
		exclaim$must_be_matchable(fn))
			
	assert(
		is.vector(coll) || is.pairlist(coll), pcall,
		exclaim$must_be_collection(coll))

	pred <- match.fun(pred)
	fn <- match.fun(fn)

	
	assert(
		xArity(fn) %in% c(1, Inf), pcall)
	
	assert(
		xArity(pred) %in% c(1, Inf), pcall)

	composite <- function (x) {
		is_match <- pred(x)
		assert(is.logical(is_match), pcall)

		if (is_match) fn(x) else x
	}

	if (length(coll) == 0) {
		list()
	} else {
		xMap(composite, coll)
	}
}
