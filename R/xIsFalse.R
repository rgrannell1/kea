
#' xIsFalse
#'
#' Is an element of a collection false?
#'
#' @param
#'    coll a collection. The collection to
#'    test each element of.
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
#'    inst/examples/example-xIsFalse.R
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

	insist$must_be_collection(coll, invoking_call)

	vapply(coll, function (x) {
		identical(x, False)
	}, logical(1), USE.NAMES = False)
}

#' @rdname xIsFalse
#' @export

xIsFalse... <- function (...) {
	xIsFalse(list(...))
}
