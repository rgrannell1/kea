

#' Generate a list of n-element lists from n collections, and apply a function to each n-element list.
#' 
#' @param fn an n-ary function, or a 
#'	 symbol or name identifying such a function.
#' @param ... n-vectors, lists or pairlists.
#'
#' @return returns a list of equal length to the shortest input collection.
#' @section Corner Cases:
#'	  the empty list is returned if the shortest collection has length-zero, or no collections
#'	  are included. Each collection is truncated to the length of the shortest collection.
#'
#' @export

xZipWith <- function (fn, ...) {
	# function -> [any] -> ... -> [[any]]
	# takes n lists/vectors, generates a list of n-tuples. 
	# returns the result of applying f to each n-tuple.
	# excess elements are discarded.

	pcall <- sys.call()
	require_a(c('function', 'string'), fn, pcall)

	coll <- list(...)
	fn <- match.fun(fn)

	stopifnot( xArity(fn) %in% c(length(coll), +Inf) )
	
	coll_lengths <- sapply(coll, length)
	min_length <- min(coll_lengths)

	if (length(coll) == 0 || min_length == 0) {
		list()
	} else {

		unname(do.call( Map, c(list(fn), 
			Map(
				function (elem) {
					head(elem, min_length)
				},
				coll
		)) ))
	}
}
