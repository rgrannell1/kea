
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
#' @examples 
#' @export

xThird <- function (coll) {
	# Collection any -> any
	# return the third element of a collection x.

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	if (length(coll) < 3) {
		stop('coll has less than three elements')
	} else {
		coll[[3]]
	}
}
