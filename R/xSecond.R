
#' Return the second value of a collection.
#'
#' @param coll a pairlist, list, or vector.
#'
#' @return the second element in \code{coll}.
#'
#' @section Corner Cases:
#'     throws an error if \code{coll} has less than two elements; this is
#'     because there is no sensible definition of the function in this case.
#'
#' @export

#| function: xSecond version: 0.1 finished: false

xSecond <- function (coll) {
	# Collection any -> any
	# return the second element of a collection x.

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	if (length(coll) < 2) {
		stop('coll has less than two elements')

	} else {
		coll[[2]]
	}
}
