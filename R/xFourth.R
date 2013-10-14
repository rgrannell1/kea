
#' xFourth
#' 
#' Return the fourth value in a collection.
#'
#' @param coll a collection/
#'
#' @return the fourth value in \code{coll}.
#'
#' @section Corner Cases:
#'	 throws an error if \code{coll} has less than four element; this is
#'	 because any other corner case would violate the functions type-signature.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xFourth <- function (coll) {
	# Collection any -> any
	# return the fourth element of a collection x.

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	fn <- dearrowise(fn)

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	assert(
		length(coll) >= 4, pcall, 
		exclaim$must_be_longer_than(coll, 3))
	
	coll[[4]]
}
