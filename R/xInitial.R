
#' xInit
#' 
#' Remove the first element from a collection.
#'
#' @param coll a collection.
#'
#' @return a list.
#'
#' @template glossary
#'
#' @examples 
#' @export

xInit <- function (coll) {
	# Collection any -> [any]
	# return everything but the first element of a 
	# collection.

	pcall <- sys.call()
	require_a(traits$collection, coll, pcall)

	if (length(coll) == 0 || length(coll) == 1) {
		list()
	} else {
		coll <- as.list(coll)
		coll[-length(coll)]
	}
}
