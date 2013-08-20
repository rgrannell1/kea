
#' Return the third value of a collection.
#'
#' @param collection a pairlist, list, or vector.
#'
#' @return the third element in \code{collection}.
#'
#' @section Corner Cases:
#'     throws an error if \code{collection} has less than three elements; this is
#'     because there is no sensible definition of the function in this case.
#'
#' @export

#| function: xThird version: 0.1 finished: false

xThird <- function (collection) {
	# Collection any -> any
	# return the third element of a listy x.

	pcall <- sys.call()
	require_a("listy", collection, pcall)

	if (length(collection) < 3) {
		stop(paste0(
			paste0(deparse(pcall), collapse = ""),
			": collection has less than three elements"))
	} else {
		collection[[3]]
	}
}
