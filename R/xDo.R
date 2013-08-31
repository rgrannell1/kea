
#' xDo
#' 
#' Map over a collection, but discard the results.
#'
#' @param fn a unary function, usually side-effectful.
#' @param coll a collection
#'
#' @return a list.
#'
#' @template glossary
#'
#' @examples 
#' @export

xDo <- function (fn, coll) {
	# apply a function to each element of a collection.
	# and discard the results.

	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	require_a("collection", coll, pcall)

	fn <- match.fun(fn)
	require_a("unary function", fn, pcall)

	if (length(coll) == 0) {
		list()
	} else {
		ith <- 1
		while (ith <= length(coll)) {
			fn( coll[[ith]] )
			ith <- ith + 1
		}
		invisible(NULL)
	}
}
