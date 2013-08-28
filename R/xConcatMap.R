
#' xConcatMap
#'
#' Concatenate the results of applying a function to each element of a collection.
#'
#' @param fn a unary function.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @export

#| function: xConcatMap version: 0.1 finished: false 

xConcatMap <- xAutoPartial(function (fn, coll) {
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
})
