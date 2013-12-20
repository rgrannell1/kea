
#' xMap
#'
#' Apply a function to each element of a collection.
#'
#' @section Uses:
#'    xMap is used to apply the same modification to every
#'    element in a collection. For example, xMap can be
#'    used to convert every integer in a list to a string,
#'    to lookup every URL in a list of URLs, or to
#'    multiply every integer in a list by two.
#'
#' @param
#'    fn a unary function.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list is \code{coll} is length-zero.
#'
#' @family
#'    higher_order_functions
#'
#' @family
#'    map_like_functions
#'
#' @family
#'    collection_functions
#'
#' @export

xMap <- function (fn, coll) {
	# (any -> any) -> Collection any -> [any]
	# map a unary function over a collection x.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

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
			lapply(coll, fn), invoking_call)
	}
}

#' @export

xMap... <- function (fn, ...) {
	xMap(fn, list(...))
}
