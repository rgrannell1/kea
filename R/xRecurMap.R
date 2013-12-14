
#' xRecurMap
#'
#' Recursively map a function into a nested collection, preserving its structure.
#'
#' @param
#'    fn a unary function.
#' @param
#'    coll a list or pairlist.
#'
#' @return
#'    a list or pairlist.
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

xRecurMap <- function (fn, coll) {
	# (any -> any) -> Recursive any -> [any]
	# Map a function into a nested collection,
	# preserving its structure.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	fn <- dearrowise(fn)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	assert(
		is_recursive(coll), invoking_call,
		exclaim$must_be_recursive(coll))

	fn <- match.fun(fn)

	recur <- function (xs) {
		# recurse into a collection.

		if (is.list(xs) || is.pairlist(xs)) {
			lapply(xs, recur)
		} else {
			try_higher_order(
				fn(xs), invoking_call)
		}
	}

	recur(as.list(coll))
}

#' @export

xRecurMap... <- function (fn, ...) {
	xRecurMap(fn, list(...))
}
