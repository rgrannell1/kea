
#' xThird
#'
#' Return the third value in a collection.
#'
#' @param
#'    coll a collection
#'
#' @return
#'    the third element in \code{coll}.
#'
#' @section Corner Cases:
#'    throws an error if \code{coll} has less than
#'    three elements; this is because any other corner
#'    case would violate the function's type-signature.
#'
#' @family
#'    collection_functions
#'
#' @export

xThird <- function (coll) {
	# Collection any -> any
	# return the third element of a collection x.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))



	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	assert(
		length(coll) >= 3, invoking_call,
		exclaim$must_be_longer_than(coll, 3))

	coll[[3]]
}

#' @export

xThird... <- function (...) {
	xThird(list(...))
}
