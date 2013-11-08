
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
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xShuffle <- function (coll) {
	# Collection any -> [any]

	parent_call <- sys.call()

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0) {
		list()
	} else {
		as.list(sample(coll))
	}
}
