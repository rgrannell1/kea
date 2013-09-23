
#' xLast
#' 
#' Return the last element in a collection.
#'
#' @param coll a collection.
#'
#' @return the value of the last element in \code{coll}.
#'
#' @section Corner Cases:
#'	 throws an error if \code{coll} has less than one element; this is
#'	 because any other corner case would violate the function's type-signature.
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R 
#' @export

xLast <- function (coll) {
	# Collection any -> any
	# return the last element of a collection x,
	# using the subset operator

	pcall <- sys.call()

	assert(
		!missing(coll), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)
	
	assert(length(coll) > 0, pcall)
	coll[[ length(coll) ]]
}
