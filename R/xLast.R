
#' xLast
#'
#' Return the last element in a collection.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    the value of the last element in \code{coll}.
#'
#' @section Corner Cases:
#'    throws an error if \code{coll} has less than one element; this is
#'    because any other corner case would violate the function's type-signature.
#'
#' @family
#'    collection_functions
#'
#' @export

xLast <- function (coll) {
	# Collection any -> any
	# return the last element of a collection x,
	# using the subset operator

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))



	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	assert(
		length(coll) > 0, invoking_call,
		exclaim$must_be_lequal_than(coll, 0))

	coll[[ length(coll) ]]
}

#' @export

xLast... <- function (...) {
	xLast(list(...))
}
