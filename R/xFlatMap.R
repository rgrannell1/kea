
#' xFlatMap
#'
#' Concatenate the results of applying a function to each element of a collection.
#'
#' @param fn a unary function.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'	 returns the empty list if \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @export

xFlatMap <- function (fn, coll) {
	# (any -> [any]) -> Collection any -> [any]
	# map unary over collection, and collate the
	# results using concatenation.

	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall, 
		exclaim$must_be_matchable(fn))

	assert(
		is.vector(coll) || is.pairlist(coll), pcall,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)
	
	assert(
		xArity(fn) %in% c(1, Inf), pcall)

	if (length(coll) == 0) {
		list()
	} else {	
		as.list( xReducel(c, lapply(coll, fn)) )
	}
}
