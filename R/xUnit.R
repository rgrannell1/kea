
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

xUnit <- MakeFun(function (coll) {
	# Collection any -> Collection
	# return the neutral element of a collection.

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (is.pairlist(coll)) {
		Null
	} else {
		unname(coll[0])
	}
})
