
#' Take several elements from the head of a collection.
#'
#' @param number a nonnegative whole number.
#' @param collection a list, vector or pairlist.
#'
#' @section Corner Cases:
#'     If \code{collection} is empty the empty list is returned.
#'
#' @return a list of elements in \code{collection}.
#' @export
#' 

#| function: xTake version: 0.1 finished: false

xTake <- function (number, collection) {
	# Collection any -> [any]
	# take the first number values of collection.

	pcall <- sys.call()
	require_a("nonnegative whole", number, pcall)
	require_a("listy", collection, pcall)

	if (length(collection) == 0 || number == 0) {
		list()
	} else {

		collection <- as.list(collection)
		collection[seq_len( min(number, length(collection)) )]
	}
}
