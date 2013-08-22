
#' Apply a function to each element of a collection.
#'
#' @param unary a unary function, or a
#'     symbol or name identifying such a function.
#' @param collection a pairlist, list, or vector.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{collection} is length-zero.
#'
#' @return a list containing \code{unary} applied to each elements of \code{collection}.
#' @export

#| function: xMap version: 0.1 finished: false 

xMap <- function (unary, collection) {
	# (any -> any) -> Collection any -> [any]
	# map a unary function over a listy x.

	pcall <- sys.call()
	require_a("functionable", unary, pcall)
	require_a("collection", collection, pcall)

	unary <- match.fun(unary)
	require_a("unary function", unary, pcall)

	if (length(collection) == 0) {
		collection
	} else {
		lapply(collection, unary)
	}
}
