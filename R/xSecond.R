
#' Return the second value of a collection.
#'
#' @param collection a pairlist, list, or vector.
#'
#' @return the second element in \code{collection}.
#'
#' @section Corner Cases:
#'     throws an error if \code{collection} has less than two elements; this is
#'     because there is no sensible definition of the function in this case.
#'
#' @export

#| function: xSecond version: 0.1 finished: false

xSecond <- function (collection) {
	# Collection any -> any
	# return the second element of a listy x.

	pcall <- sys.call()
	require_a("listy", collection, pcall)

	if (length(collection) < 2) {
		stop('collection has less than two elements')

	} else {
		collection[[2]]
	}
}
