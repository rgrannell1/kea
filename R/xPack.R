
#' xPack
#'
#' Remove all length-zero values from a collection.
#'
#' @param coll a collection.
#'
#' @return returns a list of elements in \code{coll},
#'	 with all length-zero values removed.
#'
#' @section Corner Cases:
#'	 Returns the emty list if \code{coll} is length-zero,
#'	 or all elements in \code{coll} are length-zero.
#'
#' @template glossary
#'
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xPack <- function (coll) {
	# Collection any -> [any]
	# remove all length-zero elements from a coll

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
		xReject(function (x) length(x) == 0, coll)
	}
}
