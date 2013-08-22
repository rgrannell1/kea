
#' Return the third value of a collection.
#'
#' @param coll a pairlist, list, or vector.
#'
#' @return the third element in \code{coll}.
#'
#' @section Corner Cases:
#'     throws an error if \code{coll} has less than three elements; this is
#'     because there is no sensible definition of the function in this case.
#'
#' @export

#| function: xThird version: 0.1 finished: false

xThird <- function (coll) {
	# Collection any -> any
	# return the third element of a collection x.

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	if (length(coll) < 3) {
		stop('coll has less than three elements')
	} else {
		coll[[3]]
	}
}
