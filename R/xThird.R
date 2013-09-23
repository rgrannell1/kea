
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
#' @examples inst/examples/blank.R
#' @export

xThird <- function (coll) {
	# Collection any -> any
	# return the third element of a collection x.

	pcall <- sys.call()

	assert(
		!missing(coll), pcall)

	assert(
		!missing(coll), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	assert(length(coll) >= 3, pcall)

	coll[[3]]
}
