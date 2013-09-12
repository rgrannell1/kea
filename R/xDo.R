
#' xDo
#' 
#' Map (a possibly side-effectful) function over a collection and discard the results.
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
	# function -> Collection any -> NULL
	# apply a function to each element of a collection.
	# and discard the results.

	pcall <- sys.call()
	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall)
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

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
		invisible (NULL)
	}
}
