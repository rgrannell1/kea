
#' Is an element of a collection not na?
#'
#' @param coll a list, pairlist, or vector of arbitrary values.
#'
#' @return a vector of true or false value.
#'
#' @section Corner Cases:
#'     returns false if \code{x} is length-zero.
#'
#' @export

#| function: xNotNa version: 0.1 finished: false

xNotNa <- function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not na?

	pcall <- sys.call()
	require_a("any", coll, pcall)

	unname(vapply(coll, function (x) {
		!identical(x, Na)
	}, True))
}

#' @export

xNotUnknown <- xNotNa
