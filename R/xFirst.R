
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
#' @examples 
#' @export

xFirst <- function (coll) {
	# Collection any -> any
	# return the first element of a collection x.

	pcall <- sys.call()
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	assert(length(coll) >= 1, pcall)
	coll[[1]]
}

