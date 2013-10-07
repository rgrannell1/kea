
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
#' @examples inst/examples/blank.R
#' @export

xIsEmpty <- function (coll) {
	# Collection -> boolean
	# is collection length == 0?

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is.vector(coll) || is.pairlist(coll), pcall,
		exclaim$must_be_collection(coll))

	length(coll) == 0
}
