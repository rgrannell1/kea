
#' xDo
#'
#' Map (a possibly side-effectful) function over a
#' collection and discard the results.
#'
#' @section Uses:
#'
#'    \code{xDo} behaves similarily to \code{xMap}; it applies a
#'    function to every element of a collection. However,
#'    \code{xDo} is primarily meant for use with side-effectful
#'    functions, and is more memory efficient for \code{xDo}
#'    for this task. \code{xDo} can be used for plotting
#'    every point in a list of xy coordinates, or for
#'    printing every value in a list. It is also possible
#'    to use \code{xDo} for side-effectfully updating values,
#'    but this can lead to stateful, tangled code.
#'
#' @param
#'    fn a unary function, usually side-effectful.
#'
#' @param
#'    coll a collection
#'
#' @return
#'    a list.
#'
#' @family higher_order_functions
#'
#' @family collection_functions
#'
#' @example
#'    inst/examples/example-xDo.R
#'
#' @family variadic_functions
#'
#' @rdname xDo
#' @export

xDo <- function (fn, coll) {
	# function -> Collection any -> Null
	# apply a function to each element of a collection.
	# and discard the results.

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
			fn, profile_object(fn)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	fn <- match.fun(fn)

	if (length(coll) == 0) {
		list()
	} else {
		for (ith in seq_along(coll)) {
			try_higher_order(
				fn( coll[[ith]] ), invoking_call)
		}

		invisible (Null)
	}
}

#' @rdname xDo
#' @export

xDo... <- function (fn, ...) {
	xDo(fn, list(...))
}
