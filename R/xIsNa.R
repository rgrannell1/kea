
#' Is an element of a collection na?
#'
#' @param collection a list, pairlist, or vector of arbitrary values.
#'
#' @return a vector of true or false value.
#'
#' @section Corner Cases:
#'     returns false if \code{x} is length-zero.
#'
#' @family arrow-tests
#' @export

#| function: xIsNa version: 0.1 finished: false

xIsNa <- function (collection) {
	# Collection a -> Vector boolean
	# Is an element of a collection na?

	pcall <- sys.call()
	require_a("collection", collection, pcall)

	unname(vapply(collection, function (x) {
		identical(x, NA)
	}, TRUE))
}
