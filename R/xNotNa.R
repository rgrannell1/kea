
#' Is an element of a collection not na?
#'
#' @param collection a list, pairlist, or vector of arbitrary values.
#'
#' @return a vector of true or false value.
#'
#' @section Corner Cases:
#'     returns false if \code{x} is length-zero.
#'
#' @export

#| function: xNotNa version: 0.1 finished: false

xNotNa <- function (collection) {
	# Collection a -> Vector boolean
	# Is an element of a collection not na?

	pcall <- sys.call()
	require_a("any", collection, pcall)

	unname(vapply(collection, function (x) {
		!identical(x, Na)
	}, True))
}
