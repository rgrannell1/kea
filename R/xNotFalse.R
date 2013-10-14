
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
#' @example inst/examples/blank.R
#' @export

xNotFalse <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not false?

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (x) {
			!identical(x, False)
		}, logical(1), USE.NAMES = False)		
	}
}
