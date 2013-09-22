
#' xPack
#' 
#' Remove all length-zero values from a collection.
#'
#' @param coll a collection.
#'
#' @return returns a list of elements in \code{coll}, 
#'	 with all length-zero values removed.
#'
#' @section Corner Cases:
#'	 Returns the emty list if \code{coll} is length-zero, 
#'	 or all elements in \code{coll} are length-zero.
#'
#' @template glossary
#'
#' @examples 
#' @export

xPack <- function (coll) {
	# Collection any -> [any]
	# remove all length-zero elements from a coll

	pcall <- sys.call()

	assert(
		!missing(coll), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	if (length(coll) == 0) {
		list()
	} else {
		xReject(function (x) length(x) == 0, coll)
	}
}
