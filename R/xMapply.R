
#' xMapply
#'
#' Apply a function to each element of a collection.
#'
#' @section Uses:
#'    Map is used to apply the same modification to every
#'    element in a collection. For example, map can be
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

xMapply <- function (fn, coll) {
	# map over a collection, applying each
	# function with each tuple.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	Object()


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
		lapply(coll, function (tuple) {
			do.call(fn, tuple)
		})
	}
}
