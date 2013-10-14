
#' xCombinations
#' 
#' Generate all ways of choosing several elements from a column.
#'
#' @param num a nonnegative whole number.
#' @param coll a collection
#'
#' @return a list of lists, with each list containing \code{num} elements.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{num} is zero.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xCombinations <- function (num, coll) {
	# number -> Collection

	pcall <- sys.call()
	
	assert(
		!missing(num), pcall,
		exclaim$parameter_missing(num))

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))
	
	num <- dearrowise(num)
	coll <- dearrowise(coll)

	assert(
		length(num) == 1, pcall,
		exclaim$must_have_length(num, 1))

	assert(
		is.numeric(num), pcall, 
		exclaim$must_be_numeric(num))

	assert(
		num >= 0, pcall,
		exclaim$must_be_greater_than(num, 0))

	assert(
		round(num) == num, pcall,
		exclaim$must_be_whole(num))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))	

	if (num == 0 || coll == 0) {
		list()
	} else {
		num <- min(length(coll), num)
		apply(combn(coll, num), 2, as.list)			
	}
}
