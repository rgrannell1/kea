
#'
#' Is an element of a collection true?
#'
#' @param collection a list, pairlist, or vector of arbitrary values.
#'
#' @return a vector of true or false value.
#'
#' @export

#| function: xIsTrue version: 0.1 finished: true

xIsTrue <- function (collection) {
	# a -> boolean
	# test which elements of a collection are true

	pcall <- sys.call()
	require_a("any", collection, pcall)

	vapply(collection, isTRUE, TRUE)
}
