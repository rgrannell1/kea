
#' xMap
#'
#' Apply a function to each element of a collection.
#'
#' @param
#'    fn a unary function. The function to modify each
#'    element of a collection with.
#'
#' @param
#'    coll a collection. The collection to be modified.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @family mapping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xMap.R
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

	insist $ must_be_fn_matchable(fn, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else {
		try_hof(
			lapply(coll, fn), invoking_call)
	}
}

#' @rdname xMap
#' @export

xMap... <- function (fn, ...) {
	xMap(fn, list(...))
}
