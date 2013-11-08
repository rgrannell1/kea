
#' xMapWhen
#'
#' Selectively apply a function to elements in a collection.
#'
#'
#' @param pred a predicate function.
#' @param fn a unary function.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @family higher_order_functions map_like_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xMapWhen <- function (pred, fn, coll) {
	# (any -> boolean) -> (any -> any) -> Collection any -> [any]
	# apply the function pred to collection, and apply f to
	# the elements for which pred returned true.

	parent_call <- sys.call()

	assert(
		!missing(pred), parent_call,
		exclaim$parameter_missing(pred))

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	pred <- dearrowise(pred)
	fn <- dearrowise(fn)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(pred), parent_call,
		exclaim$must_be_matchable(pred))

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	pred <- match_fn(pred)
	fn <- match_fn(fn)

	composite <- function (x) {
		is_match <- pred(x)
		assert(is.logical(is_match), parent_call)

		if (is_match) fn(x) else x
	}

	if (length(coll) == 0) {
		list()
	} else {
		xMap(composite, coll)
	}
}

#' @export

xMapWhen... <- function (pred, fn, ...) {
	xMapWhen(pred, fn, list(...))
}
