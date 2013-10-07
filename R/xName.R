
#' xName
#' 
#' Add names to a collection.
#'
#' @param coll a collection
#'
#' @return a named list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xName <- function (strs, coll) {
	# Vector string -> Collection any -> [any]
	# add names to a collection.

	pcall <- sys.call()
	
	assert(
		!missing(strs), pcall)
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is.vector(strs) || is.pairlist(strs), pcall,
		exclaim$must_be_collection(strs))
	assert(
		length(strs) == 0 || is.character(unlist(strs)), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall,
		exclaim$must_be_collection(coll))

	assert(
		length(strs) == length(coll), pcall)

	if (length(strs) == 0) {
		structure(coll, names = character(0))
	} else {
		structure(
			as.list(coll), 
			names = unlist(strs))		
	}
}
