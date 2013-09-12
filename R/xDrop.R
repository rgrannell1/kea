

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
#' @examples 
#' @export

xDrop <- function (num, coll) {
	# Collection any -> [any]
	# take the first num values of collection.
	
	pcall <- sys.call()

	assert(length(num) == 1, pcall)
	assert(is.numeric(num) && num >= 0, pcall)
	assert(round(num) == num, pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	if (length(coll) == 0 || num >= length(coll)) {
	 	list()
	} else {
		as.list(coll)[(num + 1) : length(coll)]
	}
}
