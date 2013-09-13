
#' xInter
#' 
#' Get the set intersection of two collections.
#'
#' @param coll1 a collection
#' @param coll2 a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xInter <- function (coll1, coll2) {
	# Collection any -> Collection any -> Collection any

	pcall <- sys.call()

	assert(
		is.vector(coll1) || is.pairlist(coll1), pcall)

	assert(
		is.vector(coll2) || is.pairlist(coll2), pcall)

	coll1 <- as.list(coll1)
	coll2 <- as.list(coll2)

	unique(coll1[match(coll1, coll2, 0L)])

}
