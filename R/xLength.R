
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
#'
#'
#'
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xLength <- function (coll) {
	# Collection a -> integer
	# get the length of a collection.

	parent_call <- sys.call()

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	length(coll)
}

#' @export

xLength... <- function (...) {
	xLength(list(...))
}
