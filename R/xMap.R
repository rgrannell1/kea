
#' xMap
#' 
#' Apply a function to each element of a collection.
#'
#' @param fn a unary function.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list is \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xMap <- function (fn, coll) {
	# (any -> any) -> Collection any -> [any]
	# map a unary function over a collection x.

	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)

	if (length(coll) == 0) {
		list()
	} else {
		lapply(coll, fn)
	}
}
