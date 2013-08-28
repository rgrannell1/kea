
#' Return every element in a collection except the first element.
#'
#' @param coll a pairlist, list, or vector.
#'
#' @return a list containing every element in \code{coll} except the first element.
#'
#' @section Corner Cases:
#'	 returns the empty list if \code{coll} is length-zero.
#'
#' @export

#| function: xRest version: 0.1 finished: false

xRest <- function (coll) {
	# Collection a -> [a]
	# return everything but the first element of a 
	# collection x.

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	if (length(coll) < 2) {
		list()
	} else {
		as.list( coll[-1] )
	}
}