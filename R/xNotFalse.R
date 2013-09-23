
#' xNotFalse
#' 
#' Is an element of a collection not false?
#'
#' @param coll a collection
#'
#' @return a vector of boolean values.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xNotFalse <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not false?

	pcall <- sys.call()

	assert(
		!missing(coll), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (x) {
			!identical(x, False)
		}, logical(1), USE.NAMES = False)		
	}
}
