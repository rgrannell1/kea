
#' Take several elements from the tail of a collection.
#'
#' @param number a nonnegative whole number.
#' @param collection a list, vector or pairlist.
#'
#' @section Corner Cases:
#'     If \code{collection} is empty the empty list is returned.
#'
#' @return a list of elements in \code{collection}.
#' @export

#| function: xDrop version: 0.1 finished: false

xDrop <- function (number, collection) {
	# Collection any -> [any]
	# take the first number values of collection.
	
	pcall <- sys.call()
	require_a("nonnegative whole", number, pcall)
	require_a("collection", collection, pcall)

	if (length(collection) == 0 || number >= length(collection)) {
	 	list()
	} else {
		collection <- as.list(collection)		
		collection[(number + 1) : length(collection)]
	}
}
