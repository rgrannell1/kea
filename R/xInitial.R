
#' Return every element in a collection except the last element.
#'
#' @param collection a pairlist, list, or vector.
#'
#' @return a list containing every element in \code{collection} except the last element.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{collection} is length-zero or length-one.
#'
#' @export

#| function: xInitial version: 0.1 finished: false

xInitial <- function (collection) {
	# Collection any -> [any]
	# return everything but the last element of a 
	# listy x, using the subset operator.

	pcall <- sys.call()
	require_a("listy", collection, pcall)

	if (length(collection) == 0 || length(collection) == 1) {
		list()
	} else {
		collection <- as.list(collection)
		collection[-length(collection)]
	}
}
