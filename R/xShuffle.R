
#' xShuffle
#' 
#' Permute a collection.
#'
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xShuffle <- function (coll) {
	# Collection any -> [any]

	pcall <- sys.call()
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	if (length(coll) == 0) {
		list()
	} else {
		as.list(sample(coll))
	}
}