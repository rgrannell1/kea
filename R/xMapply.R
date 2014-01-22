
#' xMapply
#'
#' Apply a function to each element of a collection.
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
#'    Returns the empty list is \code{coll} is length-zero.
#'
#' @family mapping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xMapply
#' @export

xMapply <- function (fn, coll) {
	# map over a collection, applying each
	# function with each tuple.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_fn_matchable(fn, invoking_call)
	insist$must_be_collection(coll, invoking_call)

	fn <- match_fn(fn)
	if (length(coll) == 0) {
		list()
	} else {

		lapply(coll, function (tuple) {
			do.call(fn, as.list(tuple))
		})
	}
}

#' @rdname xMapply
#' @export

xMapply... <- function (fn, ...) {
	xMapply(fn, list(...))
}
