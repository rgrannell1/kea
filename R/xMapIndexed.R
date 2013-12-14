
#' xMapIndexed
#'
#' Apply a binary function to each element of a
#' collection and its indices.
#'
#' @param
#'    fn a binary function.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list is \code{coll} is length-zero.
#'
#' @family
#'    higher_order_functions
#'
#' @family
#'    map_like_functions
#'
#' @family
#'    collection_functions
#'
#' @export

xMapIndexed <- function (fn, coll) {
	# (integer -> any -> any) -> Collection any -> [any]

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	fn <- dearrowise(fn)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)

	if (length(coll) == 0) {
		list()
	} else {

		Map(
			function (ind) {

				try_higher_order(
					fn( coll[[ind]], ind ),
					invoking_call)
			},
			seq_along(coll)
		)
	}
}

#' @export

xMapIndexed... <- function (fn, ...) {
	xMapIndexed(fn, list(...))
}
