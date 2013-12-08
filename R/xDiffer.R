
#' xDiffer
#'
#' Get the asymettric set difference of two collections.
#'
#' @param coll1 a collection
#' @param coll2 a collection
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'
#'
#'
#' @family collection_functions
#'

#' @export

xDiffer <- function (coll1, coll2) {
	# Collection any -> Collection any -> Collection any

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

	coll1 <- as.list(coll1)
	coll2 <- as.list(coll2)

	# from the base library.
	unique(
		if (length(coll1) > 0 || length(coll2) > 0) {
			coll1[match(coll1, coll2, 0L) == 0L]
		} else {
			coll1
		})
}
