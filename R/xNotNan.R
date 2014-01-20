
#' xNotNan
#'
#' Is an element of a collection not NaN?
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{coll is length-zero}.
#'
#' @template
#'    Variadic
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

	assert_is_collection(coll, invoking_call)

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
