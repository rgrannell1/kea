
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
	require_a('collection_of_length_one', strs, pcall)
	require_a('collection', coll, pcall)

	if (length(strs) != length(coll)) {
		stop("names")
	} else {

		strs <- unlist(strs)

		names(coll) <- strs
		as.list(coll)
	}
}
