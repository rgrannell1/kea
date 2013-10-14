
#' xConcat
#' 
#' Concatenate several collections into one collection.
#'
#' @param ... several collections.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     empty collections act as a unit for concatenation; concatenating the empty list 
#'    to another list returns the second list, without modification.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xConcat <- function (...) {
	# Collection any ... -> [any]
	# Concatenate several collections.

	pcall <- sys.call()

	colls <- lapply(list(...), dearrowise)

	assert(
		all(sapply(colls, is_collection)), pcall,
		exclaim$must_be_recursive_of_collections(colls))

	as.list(do.call(c, colls))

}
