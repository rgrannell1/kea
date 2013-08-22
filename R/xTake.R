
#' Take several elements from the head of a collection.
#'
#' @param num a nonnegative whole number.
#' @param coll a list, vector or pairlist.
#'
#' @section Corner Cases:
#'     If \code{coll} is empty the empty list is returned.
#'
#' @return a list of elements in \code{coll}.
#' @export
#' 

#| function: xTake version: 0.1 finished: false

xTake <- function (num, coll) {
	# Collection any -> [any]
	# take the first num values of collection.

	pcall <- sys.call()
	require_a("nonnegative whole", num, pcall)
	require_a("collection", coll, pcall)

	if (length(coll) == 0 || num == 0) {
		list()
	} else {

		coll <- as.list(coll)
		coll[seq_len( min(num, length(coll)) )]
	}
}
