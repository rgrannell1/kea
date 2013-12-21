
#' xFlatMap
#'
#' Concatenate the results of applying a function
#' to each element of a collection.
#'
#' @param
#'    fn a unary function.
#'
#' @param
#'    coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'	  returns the empty list if \code{coll}
#'    is length-zero.
#'
#' @family
#'    higher_order_functions
#'
#' @family
#     mapping_functions
#'
#' @family
#     collection_functions
#'
#' @export

xFlatMap <- function (fn, coll) {
	# (any -> [any]) -> Collection any -> [any]
	# map unary over collection, and collate the
	# results using concatenation.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)

	if (length(coll) == 0) {
		list()
	} else {
		try_higher_order(
			as.list( do.call(c, lapply(coll, fn)) ),
			invoking_call)
	}
}

#' @export

xFlatMap... <- function (fn, ...) {
	xFlatMap(fn, list(...))
}