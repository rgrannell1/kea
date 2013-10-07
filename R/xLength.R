
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
#' @examples inst/examples/blank.R
#' @export

xLength <- function (coll) {
	# Collection a -> integer
	# get the length of a collection.

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	length(coll)
}
