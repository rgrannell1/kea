
#' Is an element of a collection not true?
#'
#' @param coll a list, pairlist, or vector of arbitrary values.
#'
#' @return a vector of true or false value.
#'
#' @section Corner Cases:
#'     returns false if \code{x} is length-zero.
#'
#' @export

#| function: xNotTrue version: 0.1 finished: false

xNotTrue <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not true?

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	unname(vapply(coll, function (x) {
		!identical(x, True)
	}, True))
}
