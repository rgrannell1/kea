

#' Generate a list of n-element lists from n collections, and apply a function to each n-element list.
#'
#' @param fn an n-ary function, or a
#'	 symbol or name identifying such a function.
#' @param colls n-vectors, lists or pairlists.
#'
#' @return returns a list of equal length to the shortest input collection.
#' @section Corner Cases:
#' the empty list is returned if the shortest collection has length-zero, or no collections
#' are included. Each collection is truncated to the length of the shortest collection.
#'
#' @family higher_order_functions collection_functions
#'
#' @export

xZipWith <- function (fn, colls) {
	# function -> [any] -> ... -> [[any]]
	# takes n lists/vectors, generates a list of n-tuples.
	# returns the result of applying f to each n-tuple.
	# excess elements are discarded.

	parent_call <- sys.call()

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	colls <- lapply(colls, dearrowise)
	fn <- match_fn(fn)

	colls_lengths <- sapply(colls, length)
	min_length <- min(colls_lengths)

	if (length(colls) == 0 || min_length == 0) {
		list()
	} else {

		unname(do.call( Map, c(list(fn),
			Map(
				function (elem) {
					head(elem, min_length)
				},
				colls
		)) ))
	}
}

#' @export

xZipWith... <- function (fn, ...) {
	xZipWith(fn, list(...))
}
