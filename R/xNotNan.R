
#' xNotNan
#'
#' Is an element of a collection not NaN?
#'
#' @param
#'    coll a collection
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll is length-zero}.
#'
#' @family collection_functions
#'
#' @family variadic_functions
#'
#' @rdname xNotNan
#' @export

xNotNan <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not false?

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (x) {
			!identical(x, NaN)
		}, logical(1), USE.NAMES = False)
	}
}

#' @rdname xNotNan
#' @export

xNotNan... <- function (...) {
	xNotNan(list(...))
}
