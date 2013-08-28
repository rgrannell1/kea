
#' xNotTrue
#' 
#' Is an element of a collection not true?
#'
#' @param coll a collection.
#'
#' @return a vector of boolean values.
#'
#' @section Corner Cases: 
#'     returns logical(0) if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

#| function: xNotTrue version: 0.1 finished: false

xNotTrue <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not true?

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	if (length(coll) == 0) {
		logical(0)
	} else {
		unname(vapply(coll, function (x) {
			!identical(x, True)
		}, True))		
	}
}
