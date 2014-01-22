
#' xThirdOf
#'
#' Return the third value in a collection.
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    The third element in \code{coll}.
#'
#' @section Corner Cases:
#'    Throws an error if \code{coll} has less than
#'    three elements; this is because any other corner
#'    case would violate the function's type-signature.
#'
#' @template
#'    Variadic
#'
#' @rdname xThirdOf
#' @export

xThirdOf <- function (coll) {
	# Collection any -> any
	# return the third element of a collection x.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(coll, invoking_call)
	insist$must_be_longer_than(coll, 3, invoking_call)

	coll[[3]]
}

#' @rdname xThirdOf
#' @export

xThirdOf... <- function (...) {
	xThirdOf(list(...))
}
