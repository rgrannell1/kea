
#' Generate a list of n-element lists from a list of n collections, 
#' and apply a function to each n-element list.
#' 
#' @param fn an n-ary function, or a 
#'	 symbol or name identifying such a function.
#' @param colls a list or pairlist of n-vectors, lists or pairlists.
#'
#' @return returns a list of equal length to the shortest input collection.
#' @section Corner Cases:
#'	 the empty list is returned if the shortest collection has length-zero, or 
#'	 \code{xs} is the empty list.
#'	 Each collection is truncated to the length of the shortest collection.
#'
#' @export

#| function: xUnzipWith version: 0.1 finished: false

xUnzipWith <- function (fn, colls) {
	# (a -> b) -> [[a]] -> [[b]]
	# takes a list of n-tuples, returns n lists
	# returns the result of mapping fn over this new list. 
	# excess elements are discarded.

	pcall <- sys.call()	
	require_a('functionable', fn, pcall)
	require_a(c("list", "pairlist"), colls, pcall)

	fn <- match.fun(fn)
	colls <- as.list(colls)

	min_length <- min(sapply(colls, length))

	if (length(colls) == 0 || min_length == 0) {
		list()
	} else {

		do.call( "Map", c(list(fn), 
			Map(
				function (x) {
					head(x, min_length)
				},
				colls
		)) )
	}
}