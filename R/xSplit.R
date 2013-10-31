
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
#' @example inst/examples/blank.R
#' @export

xSplit <- function (num, coll) {
	# number -> Collection any -> [[any], [any]]
	# take the first n values of collection.

	pcall <- sys.call()

	assert(
		!missing(num), pcall,
		exclaim$parameter_missing(num))

	assert(
		!missing(coll), pcall,
		exclaim$parameter_missing(coll))

	num <- dearrowise(num)
	coll <- dearrowise(coll)

	num <- coerce_to_typed_vector(num, "numeric")
	assert(
		length(num) == 1,
		exclaim$must_have_length(num, 1))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	list(
		xTake(num, coll),
		xDrop(num, coll)
	)
}
