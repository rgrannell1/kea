
#' Remove all length-zero values from a collection.
#'
#' @param coll a list, pairlist, or vector.
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

xCompress <- function (coll) {
	# Collection any -> [any]
	# remove all length-zero elements from a coll

	pcall <- sys.call()
	require_a("listy", coll, pcall)

	if (length(coll) == 0) {
		list()
	} else {
		xReject(
			function (x) length(x) == 0,
			coll)
	}
}
