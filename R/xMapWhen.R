
#' xMapWhen
#'
#' Selectively apply a function to elements in a collection.
#'
#' @param
#'    pred a predicate function.
#'
#' @param
#'    fn a unary function.
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero.
#'
#' @family mapping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xMapWhen
#' @export

xMapWhen <- function (pred, fn, coll) {
	# (any -> boolean) -> (any -> any) -> Collection any -> [any]
	# apply the function pred to collection, and apply f to
	# the elements for which pred returned true.

	invoking_call <- sys.call()

	assert(
		!missing(pred), invoking_call,
		exclaim$parametre_missing(pred))

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_fn_matchable(pred), invoking_call,
		exclaim$must_be_matchable(
			pred, summate(pred)) )

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	pred <- match_fn(pred)
	fn <- match_fn(fn)

	composite <- function (x) {
		is_match <- pred(x)

			assert(
				is.logical(is_match), invoking_call,
				exclaim$non_logical_predicate(
					pred, summate(is_match)) )

		if (is_match) fn(x) else x
	}

	if (length(coll) == 0) {
		list()
	} else {
		try_higher_order(
			lapply(coll, composite),
			invoking_call)
	}
}

#' @rdname xMapWhen
#' @export

xMapWhen... <- function (pred, fn, ...) {
	xMapWhen(pred, fn, list(...))
}
