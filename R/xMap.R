
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
#' @examples 
#' @export

xMap <- function (fn, coll) {
	# (any -> any) -> Collection any -> [any]
	# map a unary function over a collection x.

	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	require_a("collection", coll, pcall)

	fn <- match.fun(fn)
	require_a("unary function", fn, pcall)

	if (length(coll) == 0) {
		list()
	} else {
		lapply(coll, fn)
	}
}
