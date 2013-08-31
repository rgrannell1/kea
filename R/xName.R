
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
	# add names to a collection.

	pcall <- sys.call()
	require_a('character', strs, pcall)
	require_a('collection', strs, pcall)

	if (length(strs) != length(coll)) {
		stop("names")
	} else {
		names(coll) <- strs
		as.list(coll)
	}
}
