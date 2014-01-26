
#' xUnit
#'
#' Return the empty version of a collection.
#'
#' @param
#'    coll an list, pairlist, or vector of any length.
#'
#' @return
#'    Returns null if \bold{coll} is a pairlist, a
#'    typed vector of length zero if \bold{coll}
#'	  is a vector, and the empty list if \bold{coll} is a list.
#'
#' @example
#'    inst/examples/example-xUnit.R
#'
#' @rdname xUnit
#' @export

xUnit <- function (coll) {
	# Collection any -> Collection
	# return the neutral element of a collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(coll, invoking_call)

	if (is.pairlist(coll)) {
		Null
	} else {
		unname(coll[0])
	}
}
