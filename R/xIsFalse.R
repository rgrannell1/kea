
#' Is an element of a collection false?
#'
#' @param coll a list, pairlist, or vector of arbitrary values.
#'
#' @return a vector of true or false value.
#'
#' @section Corner Cases:
#'	 returns false if \code{x} is length-zero.
#'
#' @export

#| function: xIsFalse version: 0.1 finished: false

xIsFalse <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection false?

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	unname(vapply(coll, function (x) {
		identical(x, False)
	}, TRUE))
}
