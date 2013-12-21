
#' xUnit
#'
#' Return the neutral element of a collection.
#'
#' @param
#'    coll an list, pairlist, or vector of any length.
#'
#' @return
#'    Returns null if \code{coll} is a pairlist, a
#'    typed vector of length zero if \code{coll}
#'	  is a vector, and the empty list if \code{coll} is a list.
#'
#' @family
#'    collection_functions
#'
#' @export

xUnit <- function (coll) {
	# Collection any -> Collection
	# return the neutral element of a collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	if (is.pairlist(coll)) {
		Null
	} else {
		unname(head(coll, 0))
	}
}
