
#' xSecond
#'
#' Return the second element in a collection.
#'
#' @param coll a collection
#'
#' @return the second element in \code{coll}.
#'
#' @section Corner Cases:
#'	 throws an error if \code{coll} has less than two elements; this is
#'	 because any other corner case would violate the functions type-signature.
#'
#'
#'
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xSecond <- function (coll) {
	# Collection any -> any
	# return the second element of a collection x.

	parent_call <- sys.call()

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	assert(
		length(coll) >= 2, parent_call,
		exclaim$must_be_longer_than(coll, 2))

	coll[[2]]
}

#' @export

xSecond... <- function (...) {
	xSecond(list(...))
}
