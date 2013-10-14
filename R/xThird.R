
#' xThird
#' 
#' Return the third value in a collection.
#'
#' @param coll a collection
#'
#' @return the third element in \code{coll}.
#'
#' @section Corner Cases:
#'	 throws an error if \code{coll} has less than three elements; this is
#'	 because any other corner case would violate the function's type-signature.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xThird <- function (coll) {
	# Collection any -> any
	# return the third element of a collection x.

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	assert(
		length(coll) >= 3, pcall, 
		exclaim$must_be_longer_than(coll, 3))

	coll[[3]]
}
