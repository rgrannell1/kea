
#' xMap
#'
#' Apply a function to each element of a collection.
#'
#' @section Uses:
#'     Map is used to apply the same modification to every
#'     element in a collection. For example, map can be
#'     used to convert every integer in a list to a string,
#'     to lookup every URL in a list of URLs, or to
#'     multiply every integer in a list by two.
#'
#' @param fn a unary function.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list is \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @family higher_order_functions map_like_functions
#'
#' @example inst/examples/blank.R
#' @export

xMap <- function (fn, coll) {
	# (any -> any) -> Collection any -> [any]
	# map a unary function over a collection x.

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
		lapply(coll, fn)
	}
}
