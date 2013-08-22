
#' Test if a collection is empty.
#'
#' @param coll a list, pairlist, or vector of arbitrary values.
#'
#' @return A true or false value.
#'
#' @export

#| function: xIsEmpty version: 0.1 finished: false

xIsEmpty <- function (coll) {
	# Collection -> boolean
	# is collection length == 0?

	pcall <- sys.call()
	require_a("collection", coll, pcall)

	length(coll) == 0
}
