
#' xIsNa
#'
#' Is an element of a collection na?
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a vector of boolean values.
#'
#' @family collection_functions
#'
#' @family variadic_functions
#'
#' @rdname xIsNa
#' @export

xIsNa <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection na?

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	vapply(coll, function (x) {
		identical(x, NA) ||
		identical(x, NA_integer_) ||
		identical(x, NA_real_) ||
		identical(x, NA_character_) ||
		identical(x, NA_complex_)

	}, logical(1), USE.NAMES = False)
}

#' @rdname xIsNa
#' @export

xIsNa... <- function (...) {
	xIsNa(list(...))
}
