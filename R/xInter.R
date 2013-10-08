
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
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xInter <- function (coll1, coll2) {
	# Collection any -> Collection any -> Collection any

	pcall <- sys.call()

	assert(
		!missing(coll1), pcall,
		exclaim$parameter_missing(coll1))
	
	assert(
		!missing(coll2), pcall,
		exclaim$parameter_missing(coll2))

	assert(
		is_collection(coll1), pcall,
		exclaim$must_be_collection(coll1))

	assert(
		is_collection(coll2), pcall,
		exclaim$must_be_collection(coll2))

	coll1 <- as.list(coll1)
	coll2 <- as.list(coll2)

	unique(coll1[match(coll1, coll2, 0L)])

}
