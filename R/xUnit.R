
#' Return the neutral element of a collection.
#'
#' @param coll an list, pairlist, or vector of any length.
#'
#' @return Returns null if \code{coll} is a pairlist, a typed vector of length zero if \code{coll}
#'     is a vector, and the empty list if \code{coll} is a list.
#'
#' @export

#| function: xUnit version: 0.1 finished: false

xUnit <- function (coll) {
	# Collection any -> Collection
	# return the neutral element of a collection.

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	if (is.pairlist(coll)) {
		NULL
	} else {
		unname(head(coll, 0))
	}
}
