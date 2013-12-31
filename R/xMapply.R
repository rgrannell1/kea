
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
#' @param
#'    ... see above.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list is \code{coll} is length-zero.
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

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	fn <- match.fun(fn)
	if (length(coll) == 0) {
		list()
	} else {
		lapply(coll, function (tuple) {
			do.call(fn, tuple)
		})
	}
}

#' @rdname xMapply
#' @export

xMapply... <- function (fn, ...) {
	xMapply(fn, list(...))
}
