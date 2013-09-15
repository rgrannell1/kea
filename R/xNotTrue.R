
#' xNotTrue
#' 
#' Is an element of a collection not true?
#'
#' @param coll a collection.
#'
#' @return a vector of boolean values.
#'
#' @section Corner Cases: 
#'     returns logical(0) if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples 
#' @export

xNotTrue <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not true?

	pcall <- sys.call()
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (x) {
			!identical(x, True)
		}, logical(1), USE.NAMES = False)		
	}
}
