
#' xPluck
#' 
#' de
#'
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples 
#' @export

xPluck <- function (str, coll) {
	# Vector string -> Collection any -> Collection [any]

	pcall <- sys.call()

	assert(
		is.character(str) && length(str) == 1, pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	assert(
		all( sapply(coll, is.recursive) ), pcall)

	if (length(coll) == 0) {
		list()
	} else {
		lapply( coll, function (elem) {
			as.list( elem[[str]] )
		})
	}
}
