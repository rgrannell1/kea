
#'
#' Is an element of a collection true?
#'
#' @param coll a list, pairlist, or vector of arbitrary values.
#'
#' @return a vector of true or false value.
#'
#' @export

#| function: xIsTrue version: 0.1 finished: true

xIsTrue <- function (coll) {
	# Collection a -> Vector boolean
	# test which elements of a collection are true

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	unname(vapply(coll, function (x) {
		identical(x, True)
	}, True))
}
