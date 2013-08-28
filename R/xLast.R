
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
#' @examples 
#' @export

#| function: xLast version: 0.1 finished: false

xLast <- function (coll) {
	# Collection any -> any
	# return the last element of a collection x,
	# using the subset operator

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	if (length(coll) == 0) {
		stop('cannot return the last element of the empty list')
	} else {
		coll[[length(coll)]]
	}
}
