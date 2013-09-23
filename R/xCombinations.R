
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
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R 
#' @export

xCombinations <- function (num, coll) {
	# number -> Collection

	pcall <- sys.call()
	
	assert(
		!missing(num), pcall)
	assert(
		!missing(coll), pcall)
	
	assert(
		length(num) == 1, pcall)
	assert(
		is.numeric(num) && num >= 0, pcall)
	assert(
		round(num) == num, pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)	

	if (num == 0 || coll == 0) {
		list()
	} else {
		num <- min(length(coll), num)
		apply(combn(coll, num), 2, as.list)			
	}
}
