
#' xNotNa
#'
#' Test every element in a collection for being \code{Na} of any type.
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns logical(0) if \code{coll} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @rdname xNotNa
#' @export

xNotNa <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not na?

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
		vapply(coll, function (x) {
			!identical(x, NA) &&
			!identical(x, NA_integer_) &&
			!identical(x, NA_real_) &&
			!identical(x, NA_character_) &&
			!identical(x, NA_complex_)
		}, logical(1), USE.NAMES = False)
	}
}

#' @rdname xNotNa
#' @export

xNotNa... <- function (...) {
	xNotNa(list(...))
}
