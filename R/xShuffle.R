
#' xShuffle
#' 
#' Permute a collection.
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

xShuffle <- function (coll) {
	# Collection any -> [any]

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is.vector(coll) || is.pairlist(coll), pcall,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0) {
		list()
	} else {
		as.list(sample(coll))
	}
}