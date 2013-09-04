

#' xDrop
#' 
#' Take several elements from the front of a collection.
#'
#' @param num a nonnegative whole number.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'	 If \code{coll} is empty the empty list is returned.
#'
#' @template glossary
#'
#' @examples 
#' @export

#| function: xDrop version: 0.1 finished: false

xDrop <- function (num, coll) {
	# Collection any -> [any]
	# take the first num values of collection.
	
	pcall <- sys.call()
	require_a("nonnegative whole", num, pcall)
	require_a("collection", coll, pcall)

	if (length(coll) == 0 || num >= length(coll)) {
	 	list()
	} else {
		as.list(coll)[(num + 1) : length(coll)]
	}
}
