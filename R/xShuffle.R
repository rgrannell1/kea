
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

#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xShuffle <- function (coll) {
	# Collection any -> [any]

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0) {
		list()
	} else if (length(coll) == 1)
		as.list(coll)
	else {
		as.list(sample(coll))
	}
}

#' @export

xShuffle... <- function (...) {
	xShuffle(list(...))
}