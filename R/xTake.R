
#' Take several elements from the head of a collection.
#'
#' @param num a nonnegative whole number.
#' @param collection a list, vector or pairlist.
#'
#' @section Corner Cases:
#'     If \code{collection} is empty the empty list is returned.
#'
#' @return a list of elements in \code{collection}.
#' @export
#' 

#| function: xTake version: 0.1 finished: false

xTake <- function (num, collection) {
	# Collection any -> [any]
	# take the first num values of collection.

	pcall <- sys.call()
	require_a("nonnegative whole", num, pcall)
	require_a("collection", collection, pcall)

	if (length(collection) == 0 || num == 0) {
		list()
	} else {

		collection <- as.list(collection)
		collection[seq_len( min(num, length(collection)) )]
	}
}
