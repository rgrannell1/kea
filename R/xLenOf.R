
#' xLenOf
#'
#' Get the length of a collection
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A nonnegative integer.
#'
#' @section Corner Cases:
#'      Returns zero if \bold{coll} is empty.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xLenOf.R
#'
#' @rdname xLenOf
#' @export

xLenOf <- function (coll) {
	# Collection a -> integer
	# get the length of a collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(coll, invoking_call)

	length(coll)
}

#' @rdname xLenOf
#' @export

xLenOf... <- function (...) {
	xLenOf(list(...))
}
