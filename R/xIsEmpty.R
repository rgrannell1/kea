
#' xIsEmpty
#' 
#' Is a collection length-zero?
#'
#' @param coll a collection.
#'
#' @return a boolean value.
#'
#' @template glossary
#'
#' @examples 
#' @export

xIsEmpty <- function (coll) {
	# Collection -> boolean
	# is collection length == 0?

	pcall <- sys.call()

	assert(
		!missing(coll), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	length(coll) == 0
}
