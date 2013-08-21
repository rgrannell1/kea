
#' Remove all length-zero values from a collection.
#'
#' @param collection a list, pairlist, or vector.
#'
#' @return returns a list of elements in \code{collection}, 
#'     with all length-zero values removed.
#'
#' @section Corner Cases:
#'     Returns the emty list if \code{collection} is length-zero, 
#'     or all elements in \code{collection} are length zero.
#'
#' @export

#| function: xCompress version: 0.1 finished: false

xCompress <- function (collection) {
	# Collection any -> [any]
	# remove all length-zero elements from a collection

	pcall <- sys.call()
	require_a("listy", collection, pcall)

	if (length(collection) == 0) {
		list()
	} else {
		xReject(
			function (x) length(x) == 0,
			collection)
	}
}
