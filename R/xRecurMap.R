
#' xRecurMap
#'
#' Recursively map a function into a nested collection, preserving its structure.
#'
#' @param fn a unary function.
#' @param coll a list or pairlist.
#'
#' @return a list or pairlist.
#'
#'
#'
#' @family higher_order_functions map_like_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xRecurMap <- function (fn, coll) {
	# (any -> any) -> Recursive any -> [any]
	# Map a function into a nested collection,
	# preserving its structure.

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
		is.recursive(coll), parent_call,
		exclaim$must_be_recursive(coll))

	fn <- match.fun(fn)

	recur <- function (xs) {

		if (is.list(xs) || is.pairlist(xs)) {
			lapply(xs, recur)
		} else {
			fn(xs)
		}
	}
	recur(as.list(coll))
}

#' @export

xRecurMap... <- function (fn, ...) {
	xRecurMap(fn, list(...))
}
