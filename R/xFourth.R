
#' Return the fourth value of a collection.
#'
#' @param coll a pairlist, list, or vector.
#'
#' @return the second element in \code{coll}.
#'
#' @section Corner Cases:
#'     throws an error if \code{coll} has less than four elements; this is
#'     because there is no sensible definition of the function in this case.
#'
#' @export

#| function: xFourth version: 0.1 finished: false

xFourth <- function (coll) {
	# Collection any -> any
	# return the fourth element of a collection x.

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	if (length(coll) < 4) {
		stop('coll has less than four elements')
	} else {
		coll[[4]]
	}
}
