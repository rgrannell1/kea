
#' Return every element in a collection except the last element.
#'
#' @param coll a pairlist, list, or vector.
#'
#' @return a list containing every element in \code{coll} except the last element.
#'
#' @section Corner Cases:
#'	 returns the empty list if \code{coll} is length-zero or length-one.
#'
#' @export

#| function: xInit version: 0.1 finished: false

xInit <- function (coll) {
	# Collection any -> [any]
	# return everything but the last element of a 
	# collection x, using the subset operator.

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	if (length(coll) == 0 || length(coll) == 1) {
		list()
	} else {
		coll <- as.list(coll)
		coll[-length(coll)]
	}
}
