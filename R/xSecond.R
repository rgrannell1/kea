
#' xSecond
#'
#' Return the second element in a collection.
#'
#' @param
#'    coll a collection
#'
#' @return
#'    the second element in \code{coll}.
#'
#' @section Corner Cases:
#'    throws an error if \code{coll} has less than two
#'    elements; this is because any other corner case
#'    would violate the functions type-signature.
#'
#' @family collection_functions
#'
#' @export

xSecond <- function (coll) {
	# Collection any -> any
	# return the second element of a collection x.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	assert(
		length(coll) >= 2, invoking_call,
		exclaim$must_be_longer_than(
			coll, 2, profile_object(coll)) )

	coll[[2]]
}

#' @export

xSecond... <- function (...) {
	xSecond(list(...))
}
