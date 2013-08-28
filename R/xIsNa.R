
#' Is an element of a collection na?
#'
#' @param coll a list, pairlist, or vector of arbitrary values.
#'
#' @return a vector of true or false value.
#'
#' @section Corner Cases:
#'	 returns false if \code{x} is length-zero.
#'
#' @family arrow-tests
#' @export

#| function: xIsNa version: 0.1 finished: false

xIsNa <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection na?

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	unname(vapply(coll, function (x) {
		identical(x, NA)
	}, TRUE))
}
