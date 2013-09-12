
#' xName
#' 
#' Add names to a collection.
#'
#' @param coll a collection
#'
#' @return a named list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xName <- function (strs, coll) {
	# Vector string -> Collection any -> [any]
	# add names to a collection.

	pcall <- sys.call()
	
	assert(
		is.vector(strs) || is.pairlist(strs) && 
		is.character(unlist(strs)), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	assert(length(strs) == length(coll), pcall)

	structure(
		as.list(coll), 
		names = unlist(strs))
}
