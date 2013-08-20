
#' Return the first value of a collection.
#'
#' @param collection a pairlist, list, or vector.
#'
#' @return the first element in \code{collection}.
#'
#' @section Corner Cases:
#'     throws an error if \code{collection} has less than one element; this is
#'     because any other corner case would violate the functions type-signature.
#'
#' @family arrow-selects
#' @export

#| function: xFirst version: 0.1 finished: false

xFirst <- function (collection) {
	# Collection any -> any
	# return the first element of a listy x.

	pcall <- sys.call()
	require_a("listy", collection, pcall)

	if (length(collection) < 1) {
		stop('collection has less than one element')
	} else {
		collection[[1]]
	}
}

