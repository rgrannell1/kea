
#' Concatenate the results of applying a function to each element of a collection.
#'
#' @param fn a unary function, or a
#'     symbol or name identifying such a function.
#' @param coll a pairlist, list, or vector.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'
#' @return a list containing \code{fn} applied to some elements of \code{coll}.
#' @family arrow-maps
#' @export

#| function: xConcatMap version: 0.1 finished: false 

xConcatMap <- function (fn, coll) {
	# (any -> [any]) -> Collection any -> [any]
	# map unary over collection, and collate the
	# results using concatenation.

	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	require_a("listy", coll, pcall)

	fn <- match.fun(fn)
	require_a("unary function", fn)

	if (length(coll) == 0) {
		list()
	} else {	
		as.list( xReducel(c, lapply(coll, fn)) )
	}
}
