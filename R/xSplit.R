
#' xSplit
#' 
#' Split a collection into elements before and upto an index, and after that index.
#'
#' @param num a nonnegative whole number.
#' @param coll a collection
#'
#' @return a list of two lists; the first list containing the first 
#'	 \code{num} elements of \code{coll}, and the second list containing the 
#'	 remaining elements \code{coll}.
#'
#' @section Corner Cases:
#'	 If \code{num} is zero then the first list in the returned value is empty.
#'	 Likewise, if \code{num} is equal or larger than the length of \code{coll} then
#'	 the second return list is empty. If \code{coll} is length zero both lists are empty.
#'
#' @template glossary
#'
#' @examples 
#' @export

xSplit <- function (num, coll) {
	# number -> Collection any -> [[any], [any]]
	# take the first n values of collection.

	pcall <- sys.call()

	assert(
		!missing(num), pcall)
	assert(
		!missing(coll), pcall)

	assert(
		length(num) == 1, pcall)
	assert(
		is.numeric(num), pcall)
	assert(
		num >= 0, pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	list(
		xTake(num, coll),
		xDrop(num, coll)
	)
}
