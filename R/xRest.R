
#' xRest
#'
#' Return a list excluding the first element of a collection.
#'
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'
#'
#' @family collection_functions
#'

#' @export

xRest <- function (coll) {
	# Collection a -> [a]
	# return everything but the first element of a
	# collection x.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	if (length(coll) < 2) {
		list()
	} else {
		as.list( coll[-1] )
	}
}

#' @export

xRest... <- function (...) {
	xRest(list(...))
}
