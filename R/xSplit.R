
#' xSplit
#' 
#' Split a collection into elements before and upto an index, and after that index.
#'
#' @param ind a nonnegative whole number.
#' @param coll a collection
#'
#' @return a list of two lists; the first list containing the first 
#'	 \code{ind} elements of \code{coll}, and the second list containing the 
#'	 remaining elements \code{coll}.
#'
#' @section Corner Cases:
#'	 If \code{ind} is zero then the first list in the returned value is empty.
#'	 Likewise, if \code{ind} is equal or larger than the length of \code{coll} then
#'	 the second return list is empty. If \code{coll} is length zero both lists are empty.
#'
#' @template glossary
#'
#' @examples 
#' @export

xSplit <- function (ind, coll) {
	# number -> Collection any -> [[any], [any]]
	# take the first n values of collection.

	pcall <- sys.call()
	require_a("nonnegative whole", ind, pcall)
	require_a("collection", coll, pcall)

	list(
		xTake(ind, coll),
		xDrop(ind, coll)
	)
}
