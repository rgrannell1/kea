
#' xFlatMap
#'
#' Concatenate the results of applying a function to each element of a collection.
#'
#' @param fn a unary function.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'	 returns the empty list if \code{coll} is length-zero.
#'
#'
#'
#' @family higher_order_functions map_like_functions collection_functions
#'
#' @export

xFlatMap <- function (fn, coll) {
	# (any -> [any]) -> Collection any -> [any]
	# map unary over collection, and collate the
	# results using concatenation.

	parent_call <- sys.call()

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	fn <- dearrowise(fn)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else {
		as.list( do.call(c, lapply(coll, fn)) )
	}
}

#' @export

xFlatMap... <- function (fn, ...) {
	xFlatMap(fn, list(...))
}