
#' xElemIsNa
#'
#' Is an element of a collection na?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being na.
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
#'    inst/examples/example-xElemIsNa.R
#'
#' @rdname xElemIsNa
#' @export

xElemIsNa <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection na?

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)
	insist $ must_be_collection(coll, invoking_call)

	vapply(coll, function (x) {
		identical(x, NA) ||
		identical(x, NA_integer_) ||
		identical(x, NA_real_) ||
		identical(x, NA_character_) ||
		identical(x, NA_complex_)

	}, logical(1), USE.NAMES = False)
}

#' @rdname xElemIsNa
#' @export

xElemIsNa... <- function (...) {
	xElemIsNa(list(...))
}
