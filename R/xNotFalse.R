
#' xNotFalse
#' 
#' Is an element of a collection not false?
#'
#' @param coll a collection
#'
#' @return a vector of boolean values.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

#| function: xNotFalse version: 0.1 finished: false

xNotFalse <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not false?

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (x) {
			!identical(x, False)
		}, logical(1), USE.NAMES = False)		
	}
}
