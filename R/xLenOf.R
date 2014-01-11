
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
#'      Returns zero if \code{coll} is empty.
#'
#' @template
#'    Variadic
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

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	length(coll)
}

#' @rdname xLenOf
#' @export

xLenOf... <- function (...) {
	xLenOf(list(...))
}
