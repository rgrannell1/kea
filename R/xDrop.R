
#' xDrop
#' 
#' Take several elements from the front of a collection.
#'
#' @param num a nonnegative whole number.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'	 If \code{coll} is empty the empty list is returned.
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xDrop <- function (num, coll) {
	# Collection any -> [any]
	# take the first num values of collection.
	
	pcall <- sys.call()

	assert(
		!missing(num), pcall,
		exclaim$parameter_missing(num))
	
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		length(num) == 1, pcall,
		exclaim$must_have_length(num, 1))
	
	assert(
		is.numeric(num) && num >= 0, pcall,
		exclaim$must_be_numeric(num))
	
	assert(
		round(num) == num, pcall,
		exclaim$must_be_whole(num))

	assert(
		is.vector(coll) || is.pairlist(coll), pcall,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0 || num >= length(coll)) {
	 	list()
	} else {
		as.list(coll)[(num + 1) : length(coll)]
	}
}
