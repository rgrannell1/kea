
#' xSplitAt
#' 
#' Split a collection into elements before and upto an index, and after that index.
#'
#' @param coll a collection
#'
#' @return a list of two lists; the first list containing the first 
#'	 \code{ith} elements of \code{coll}, and the second list containing the 
#'	 remaining elements \code{coll}.
#'
#' @section Corner Cases:
#'	 If \code{ith} is zero then the first list in the returned value is empty.
#'	 Likewise, if \code{ith} is equal or larger than the length of \code{coll} then
#'	 the second return list is empty. If \code{coll} is length zero both lists are empty.
#'
#' @template glossary
#'
#' @examples 
#' @export

#| function: xSplitAt version: 0.1 finished: false

xSplitAt <- function (ith, coll) {
	# number -> Collection any -> [[any], [any]]
	# take the first n values of collection.

	pcall <- sys.call()
	require_a("nonnegative whole", ith, pcall)
	require_a("collection", coll, pcall)

	list(
		xTake(ith, coll),
		xDrop(ith, coll)
	)
}
