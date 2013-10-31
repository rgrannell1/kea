
#' xUnit
#'
#' Return the neutral element of a collection.
#'
#' @param coll an list, pairlist, or vector of any length.
#'
#' @return Returns null if \code{coll} is a pairlist, a typed vector of length zero if \code{coll}
#'	 is a vector, and the empty list if \code{coll} is a list.
#'
#' @export

xUnit <- function (coll) {
	# Collection any -> Collection
	# return the neutral element of a collection.

	pcall <- sys.call()

	assert(
		!missing(coll), pcall,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	if (is.pairlist(coll)) {
		Null
	} else {
		unname(head(coll, 0))
	}
}
