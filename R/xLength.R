
#' xLength
#' 
#' Get the length of a collection
#'
#' @param coll a collection
#'
#' @return a nonnegative integer.
#'
#' @section Corner Cases: 
#'     returns 0 if \code{coll} is empty.
#' @template glossary
#'
#' @examples 
#' @export

xLength <- function (coll) {
	# Collection a -> integer
	# 

	pcall <- sys.call()
	require_a('collection', coll, pcall)

	length(pcall)
}
