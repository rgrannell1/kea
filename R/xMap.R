
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
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{coll} is length-zero.
#'
#' @family mapping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xMap
#' @export

xMap <- function (fn, coll) {
	# (any -> any) -> Collection any -> [any]
	# map a unary function over a collection x.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert_is_fn_matchable(fn, invoking_call)

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else {
		try_higher_order(
			lapply(coll, fn), invoking_call)
	}
}

#' @rdname xMap
#' @export

xMap... <- function (fn, ...) {
	xMap(fn, list(...))
}
