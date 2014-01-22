

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
#'    A list or pairlist.
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

	insist$must_be_fn_matchable(coll, invoking_call)
	insist$must_be_recursive(coll, invoking_call)

	fn <- match_fn(fn)

	recur <- function (xs) {
		# recurse into a collection.

		if (is.list(xs) || is.pairlist(xs)) {
			lapply(xs, recur)
		} else {
			try_hof(
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
