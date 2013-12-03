#' xDo
#'
#' Map (a possibly side-effectful) function over a collection and discard the results.
#'
#' @section Uses:
#'
#' \code{xDo} behaves similarily to \code{xMap}; it applies a
#' function to every element of a collection. However,
#' \code{xDo} is primarily meant for use with side-effectful
#' functions, and is more memory efficient for \code{xDo}
#' for this task. \code{xDo} can be used for plotting
#' every point in a list of xy coordinates, or for
#' printing every value in a list. It is also possible
#' to use \code{xDo} for side-effectfully updating values,
#' but this can lead to stateful, tangled code.
#'
#' @param fn a unary function, usually side-effectful.
#' @param coll a collection
#'
#' @return a list.
#'
#'
#'
#' @family higher_order_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xDo <- function (fn, coll) {
	# function -> Collection any -> Null
	# apply a function to each element of a collection.
	# and discard the results.

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
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)

	if (length(coll) == 0) {
		list()
	} else {
		for (ith in seq_along(coll)) {
			try_higher_order(
				fn( coll[[ith]] ), parent_call)
		}
		invisible (Null)
	}
}

#' @export

xDo... <- function (fn, ...) {
	xDo(fn, list(...))
}
