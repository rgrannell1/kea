
#' Return the neutral element of a collection.
#'
#' @param collection an list, pairlist, or vector of any length.
#'
#' @return Returns null if collection is a pairlist, a typed vector of length zero if \code{collection}
#'     is a vector, and the empty list if collection is a list.
#'
#' @export

#| function: xUnit version: 0.1 finished: false

xUnit <- function (collection) {
	# Collection any -> Collection
	# return the neutral element of a collection.

	pcall <- sys.call()
	require_a("listy", collection, pcall)

	if (is.pairlist(collection)) {
		NULL
	} else {
		unname(head(collection, 0))
	}
}
