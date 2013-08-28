
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

#| function: xIsEmpty version: 0.1 finished: false

xIsEmpty <- function (coll) {
	# Collection -> boolean
	# is collection length == 0?

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	length(coll) == 0
}
