
#' xRest
#' 
#' Return a list excluding the first element of a collection.
#'
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xRest <- function (coll) {
	# Collection a -> [a]
	# return everything but the first element of a 
	# collection x.

	pcall <- sys.call()

	assert(
		!missing(coll), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	if (length(coll) < 2) {
		list()
	} else {
		as.list( coll[-1] )
	}
}