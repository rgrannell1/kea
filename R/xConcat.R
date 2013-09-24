
#' xConcat
#' 
#' Concatenate several collections into one collectino.
#'
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     empty collections act as a unit for concatenation; concatenating the empty list 
#'    to another list returns the second list, without modification.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xConcat <- function (...) {
	# Collection any ... -> [any]
	# Concatenate several collections.

	pcall <- sys.call()

	colls <- list(...)

	assert(
		all( sapply(colls, function (coll) {
			is.vector(coll) || is.pairlist(coll) 
		}) ), pcall)

	as.list(do.call(c, colls))

}