
#' Apply a function to each element of a collection.
#'
#' @param fn a unary function, or a
#'	 symbol or name identifying such a function.
#' @param coll a pairlist, list, or vector.
#'
#' @section Corner Cases:
#'	 returns the empty list if \code{coll} is length-zero.
#'
#' @return a list containing \code{fn} applied to each elements of \code{coll}.
#' @export

#| function: xMap version: 0.1 finished: false 

xMap <- function (fn, coll) {
	# (any -> any) -> Collection any -> [any]
	# map a unary function over a listy x.

	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	require_a("collection", coll, pcall)

	fn <- match.fun(fn)
	require_a("unary function", fn, pcall)

	if (length(coll) == 0) {
		coll
	} else {
		lapply(coll, fn)
	}
}
