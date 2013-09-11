
#' xSecond
#' 
#' Return the second element in a collection.
#'
#' @param coll a collection
#'
#' @return the second element in \code{coll}.
#'
#' @section Corner Cases:
#'	 throws an error if \code{coll} has less than two elements; this is
#'	 because any other corner case would violate the functions type-signature.
#'
#' @template glossary
#'
#' @examples 
#' @export

xSecond <- function (coll) {
	# Collection any -> any
	# return the second element of a collection x.

	pcall <- sys.call()
	require_a(traits$collection, coll, pcall)

	if (length(coll) < 2) {
		stop('coll has less than two elements')

	} else {
		coll[[2]]
	}
}
