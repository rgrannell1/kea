
#' Return the fourth value of a collection.
#'
#' @param collection a pairlist, list, or vector.
#'
#' @return the second element in \code{collection}.
#'
#' @section Corner Cases:
#'     throws an error if \code{collection} has less than four elements; this is
#'     because there is no sensible definition of the function in this case.
#'
#' @export

#| function: xFourth version: 0.1 finished: false

xFourth <- function (collection) {
	# Collection any -> any
	# return the fourth element of a collection x.

	pcall <- sys.call()
	require_a("collection", collection, pcall)

	if (length(collection) < 4) {
		stop('collection has less than four elements')
	} else {
		collection[[4]]
	}
}
