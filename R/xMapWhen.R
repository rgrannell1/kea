
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
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

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
