
#' xUnit
#'
#' Return the empty version of a collection.
#'
#' @param
#'    coll an list, pairlist, or vector of any length.
#'    The collection to return the length-zero
#'    unit element of.
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

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(coll, invoking_call)

	if (is.pairlist(coll)) {
		Null
	} else {
		unname(coll[0])
	}
}
