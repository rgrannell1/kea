
#' xDiffer
#' 
#' Get the asymettric set difference of two collections.
#'
#' @param coll1 a collection
#' @param coll2 a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples 
#' @export

xDiffer <- function (coll1, coll2) {
	# Collection any -> Collection any -> Collection any

	pcall <- sys.call()

	assert(
		!missing(coll1), pcall)
	assert(
		!missing(coll2), pcall)

	assert(
		is.vector(coll1) || is.pairlist(coll1), pcall)
	
	assert(
		is.vector(coll2) || is.pairlist(coll2), pcall)

	coll1 <- as.list(coll1)
	coll2 <- as.list(coll2)

	# from the base library.
	unique(
		if (length(coll1) > 0 || length(coll2) > 0) {
			coll1[match(coll1, coll2, 0L) == 0L]
		} else {
			coll1
		})
}
