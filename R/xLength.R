
#' xLength
#'
#' Get the length of a collection
#'
#' @param
#'    coll a collection
#'
#' @return
#'    a nonnegative integer.
#'
#' @section Corner Cases:
#'     returns zero if \code{coll} is empty.
#'
#' @family collection_functions
#'
#' @export

xLength <- function (coll) {
	# Collection a -> integer
	# get the length of a collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	length(coll)
}

#' @export

xLength... <- function (...) {
	xLength(list(...))
}
