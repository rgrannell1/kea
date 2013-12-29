
#' xNotTrue
#'
#' Is an element of a collection not true?
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a vector of boolean values.
#'
#' @section Corner Cases:
#'    returns logical(0) if \code{coll} is length-zero.
#'
#' @family collection_functions
#'
#' @family variadic_functions
#'
#' @rdname xNotTrue
#' @export

xNotTrue <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not true?

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
			!identical(x, True)
		}, logical(1), USE.NAMES = False)
	}
}

#' @rdname xNotTrue
#' @export

xNotTrue... <- function (...) {
	xNotTrue(list(...))
}
