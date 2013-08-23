
#' Selectively apply a function to each element of a collection.
#'
#' @param pred a unary function that returns a logical value, or a 
#'     symbol or name identifying such a function.
#' @param fn a unary function, or a
#'     symbol or name identifying such a function.
#' @param coll a pairlist, list, or vector.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'
#' @return a list containing \code{f} applied to some elements of \code{coll}.
#' @family arrow-maps
#' @export

#| function: xMapWhen version: 0.1 finished: false 

xMapWhen <- function (pred, fn, coll) {
	# (any -> boolean) -> (any -> any) -> Collection any -> [any]
	# apply the function pred to collection, and apply f to
	# the elements for which pred returned true.

	pcall <- sys.call()
	require_a("functionable", pred, pcall)
	require_a("functionable", fn, pcall)
	require_a("collection", coll, pcall)

	pred <- match.fun(pred)
	fn <- match.fun(fn)

	require_a("unary function", pred, pcall)
	require_a("unary function", fn, pcall)

	composite <- function (x) {
		is_match <- pred(x)
		stopifnot(is.logical(is_match))

		if (is_match) fn(x) else x
	}

	if (length(coll) == 0) {
		list()
	} else {
		xMap(composite, coll)
	}
}
