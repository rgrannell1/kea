
#' Split a collection into elements before and upto an index, and after that index.
#'
#' @param collection a pairlist, list, or vector.
#'
#' @return a list of two lists; the first list containing the first 
#'     \code{ith} elements of \code{collection}, and the second list containing the 
#'     remaining elements \code{collection}.
#'
#' @section Corner Cases:
#'     If \code{ith} is zero then the first list in the returned value is empty.
#'     Likewise, if \code{ith} is equal or larger than the length of \code{collection} then
#'     the second return list is empty. If \code{collection} is length zero both lists are empty.
#'
#' @export

#| function: xSplitAt version: 0.1 finished: false

xSplitAt <- function (ith, collection) {
	# Collection any -> [[any], [any]]
	# take the first n values of collection.

	pcall <- sys.call()
	require_a("nonnegative whole", ith, pcall)
	require_a("collection", collection, pcall)

	list(
		xTake(ith, collection),
		xDrop(ith, collection)
	)
}
