
#' xUnion
#'
#' Get the set union of two collections.
#'
#' @param coll1 a collection
#' @param coll2 a collection
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.

#' @family collection_functions
#'

#' @export

xUnion <- function (coll1, coll2) {
	# Collection any -> Collection any -> Collection any
	# get the set union of two collections.

	invoking_call <- sys.call()

	assert(
		!missing(coll1), invoking_call,
		exclaim$parameter_missing(coll1))

	assert(
		!missing(coll2), invoking_call,
		exclaim$parameter_missing(coll2))

	coll1 <- dearrowise(coll1)
	coll2 <- dearrowise(coll2)

	assert(
		is_collection(coll1), invoking_call,
		exclaim$must_be_collection(coll1))

	assert(
		is_collection(coll2), invoking_call,
		exclaim$must_be_collection(coll2))

	unique( c(as.list(coll1), as.list(coll2)) )
}
