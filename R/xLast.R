
#' Return the last element in a collection.
#'
#' @param collection a pairlist, list, or vector.
#'
#' @return the value of the last element in \code{collection}.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{collection} is length-zero.
#'
#' @export

#| function: xLast version: 0.1 finished: false

xLast <- function (collection) {
	# Collection any -> any
	# return the last element of a listy x,
	# using the subset operator

	pcall <- sys.call()
	require_a("listy", collection, pcall)

	if (length(collection) == 0) {
		stop('cannot return the last element of the empty list')
	} else {
		collection[[length(collection)]]
	}
}
