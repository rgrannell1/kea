
#' Is an element of a collection not true?
#'
#' @param collection a list, pairlist, or vector of arbitrary values.
#'
#' @return a vector of true or false value.
#'
#' @section Corner Cases:
#'     returns false if \code{x} is length-zero.
#'
#' @export

#| function: xNotTrue version: 0.1 finished: false

xNotTrue <- function (collection) {
	# Collection a -> Vector boolean
	# Is an element of a collection not true?

	pcall <- sys.call()
	require_a("listy", collection, pcall)

	unname(vapply(collection, function (x) {
		!identical(x, TRUE)
	}, TRUE))
}
