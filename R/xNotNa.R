
#' xNotNa
#' 
#' Is an element of a collection not na?
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

#| function: xNotNa version: 0.1 finished: false

xNotNa <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not na?

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	if (length(coll) == 0) {
		logical(0)
	} else {
		unname(vapply(coll, function (x) {
			!identical(x, Na)
		}, True))		
	}
}
