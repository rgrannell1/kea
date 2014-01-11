
#' xIsNan
#'
#' Is an element in a collection NaN?
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns True if coll is length-zero.
#'
#' @template
#'    Variadic
#'
#' @rdname xIsNan
#' @export

xIsNan <- function (coll) {
	# collection any -> vector Boolean

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	if (length(coll) == 0) {
		logical(0)
	} else {
		res <- vector(mode = 'logical', length(coll))

		for (ith in seq_along(coll)) {
			res[ith] <- identical(coll[[ith]], NaN)
		}

		res
	}
}

#' @rdname xIsNan
#' @export

xIsNan... <- function (...) {
	xIsNan(list(...))
}
