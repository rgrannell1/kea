
#' Split a collection into elements before and upto an index, and after that index.
#'
#' @param collection a pairlist, list, or vector.
#'
#' @return a list of two lists; the first list containing the first 
#'     \code{number} elements of \code{collection}, and the second list containing the 
#'     remaining elements \code{collection}.
#'
#' @section Corner Cases:
#'     If \code{number} is zero then the first list in the returned value is empty.
#'     Likewise, if \code{number} is equal or larger than the length of \code{collection} then
#'     the second return list is empty. If \code{collection} is length zero both lists are empty.
#'
#' @export

#| function: xSplitAt version: 0.1 finished: false

xSplitAt <- function (number, collection) {
	# Collection any -> [[any], [any]]
	# take the first n values of collection.

	pcall <- sys.call()
	require_a("nonnegative whole", number, pcall)
	require_a("listy", collection, pcall)

	list(
		xTake(number, collection),
		xDrop(number, collection)
	)
}
