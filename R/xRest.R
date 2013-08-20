
#' Return every element in a collection except the first element.
#'
#' @param collection a pairlist, list, or vector.
#'
#' @return a list containing every element in \code{collection} except the first element.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{collection} is length-zero.
#'
#' @export

#| function: xRest version: 0.1 finished: false

xRest <- function (collection) {
	# [a] -> [a]
	# return everything but the first element of a 
	# listy x.

	pcall <- sys.call()
	require_a("listy", collection, pcall)

	if (length(collection) < 2) {
		list()
	} else {
		as.list( collection[-1] )
	}
}