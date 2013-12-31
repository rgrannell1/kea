
#' xIsFalse
#'
#' Is an element of a collection false?
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a vector of boolean values.
#'
#' @family collection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xIsFalse
#' @export

xIsFalse <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection false?

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	vapply(coll, function (x) {
		identical(x, False)
	}, logical(1), USE.NAMES = False)
}

#' @rdname xIsFalse
#' @export

xIsFalse... <- function (...) {
	xIsFalse(list(...))
}
