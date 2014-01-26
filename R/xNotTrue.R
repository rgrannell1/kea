
#' xNotTrue
#'
#' Is an element of a collection not true?
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
#'    Returns logical(0) if \bold{coll} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xNotTrue.R
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

	insist$must_be_collection(coll, invoking_call)

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
