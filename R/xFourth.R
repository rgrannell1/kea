
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
#' @examples 
#' @export

xFourth <- function (coll) {
	# Collection any -> any
	# return the fourth element of a collection x.

	pcall <- sys.call()
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	if (length(coll) < 4) {
		stop('coll has less than four elements')
	} else {
		coll[[4]]
	}
}
