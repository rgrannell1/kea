
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
	require_a(traits$collection, coll, pcall)

	length(coll) == 0
}
