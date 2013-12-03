
#' xMapIndexed
#'
#' Apply a binary function to each element of a collection and its indices.
#'
#' @param fn a binary function.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list is \code{coll} is length-zero.
#'
#'
#'
#' @family higher_order_functions map_like_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xMapIndexed <- function (fn, coll) {
	# (integer -> any -> any) -> Collection any -> [any]

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
		Map(
			function (ind) {

				try_higher_order(
					fn( coll[[ind]], ind ),
					parent_call)

			},
			seq_along(coll)
		)
	}
}

#' @export

xMapIndexed... <- function (fn, ...) {
	xMapIndexed(fn, list(...))
}
