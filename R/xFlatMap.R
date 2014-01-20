
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
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'	  Returns the empty list if \code{coll} is length-zero.
#'
#' @family mapping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xFlatMap
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

	assert_is_fn_matchable(fn, invoking_call)

	assert_is_collection(coll, invoking_call)

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else {
		try_higher_order(
			as.list( do.call(c, lapply(coll, fn)) ),
			invoking_call)
	}
}

#' @rdname xFlatMap
#' @export

xFlatMap... <- function (fn, ...) {
	xFlatMap(fn, list(...))
}