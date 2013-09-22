
#' xLength
#' 
#' Get the length of a collection
#'
#' @param coll a collection
#'
#' @return a nonnegative integer.
#'
#' @section Corner Cases: 
#'     returns 0 if \code{coll} is empty.
#' @template glossary
#'
#' @examples 
#' @export

xLength <- function (coll) {
	# Collection a -> integer
	# get the length of a collection.

	pcall <- sys.call()

	assert(
		!missing(coll), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	length(pcall)
}
