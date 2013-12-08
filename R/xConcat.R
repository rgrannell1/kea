
#' xConcat
#'
#' Concatenate several collections into one collection.
#'
#' @param colls several collections.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     empty collections act as a unit for concatenation; concatenating the empty list
#'    to another list returns the second list, without modification.
#'

#' @export

xConcat <- function (colls) {
	# Collection any coll -> [any]
	# Concatenate several collections.

	invoking_call <- sys.call()

	colls <- lapply(colls, dearrowise)

	assert(
		all(sapply(colls, is_collection)), invoking_call,
		exclaim$must_be_recursive_of_collections(colls))

	as.list(do.call(c, colls))
}

#' @export

xConcat... <- function (...) {
	xConcat(list(...))
}
