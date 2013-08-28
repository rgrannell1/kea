
#' Return the last element in a collection.
#'
#' @param coll a pairlist, list, or vector.
#'
#' @return the value of the last element in \code{coll}.
#'
#' @section Corner Cases:
#'	 returns the empty list if \code{coll} is length-zero.
#'
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
