
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
	require_a('collection_of_string', strs, pcall)
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	if (length(strs) != length(coll)) {
		stop ("strs and coll should have equal length.")
	} else {	
		structure(
			as.list(coll), 
			names = unlist(strs))
	}
}
