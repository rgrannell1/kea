

#' xDeepMap
#'
#' Recursively map a function into a nested collection,
#' preserving its structure.
#'
#' @param
#'    fn a unary function.
#'
#' @param
#'    coll a list or pairlist.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    a list or pairlist.
#'
#' @family mapping_functions
#'
#'
#' @template
#'    Variadic
#'
#' @rdname xDeepMap
#' @export

xDeepMap <- function (fn, coll) {
	# (any -> any) -> Recursive any -> [any]
	# Map a function into a nested collection,
	# preserving its structure.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	assert(
		is_recursive(coll), invoking_call,
		exclaim$must_be_recursive(
			coll, summate(coll)) )

	fn <- match_fn(fn)

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

#' @rdname xDeepMap
#' @export

xDeepMap... <- function (fn, ...) {
	xDeepMap(fn, list(...))
}
