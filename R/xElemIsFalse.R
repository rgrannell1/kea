
#' xElemIsFalse
#'
#' Is an element of a collection false?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being false.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemIsFalse.R
#'
#' @rdname xElemIsFalse
#' @export

xElemIsFalse <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection false?

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist $ must_be_collection(coll, invoking_call)

	vapply(coll, function (x) {
		identical(x, False)
	}, logical(1), USE.NAMES = False)
}

#' @rdname xElemIsFalse
#' @export

xElemIsFalse... <- function (...) {
	xElemIsFalse(list(...))
}
