
#' xFirst
#' 
#' Return the first element of a collection.
#'
#' @param coll a collection
#'
#' @return the first element in \code{coll}.
#'
#' @section Corner Cases:
#'	 throws an error if \code{coll} has less than one element; this is
#'	 because any other corner case would violate the functions type-signature.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xFirst <- function (coll) {
	# Collection any -> any
	# return the first element of a collection x.

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	assert(
		length(coll) >= 1, pcall,
		exclaim$must_be_longer_than(coll, 1))
	
	coll[[1]]
}

