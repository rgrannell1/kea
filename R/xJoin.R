
#' xJoin
#'
#' Concatenate several collections into one collection.
#'
#' @param
#'    colls several collections.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    empty collections act as a unit for concatenation;
#'    concatenating the empty list to another list returns
#'    the second list, without modification.
#'
#' @family collection_functions
#'
#' @family reshaping_functions
#'
#' @export

xJoin <- function (colls) {
	# Collection any coll -> [any]
	# Concatenate several collections
	# into one list.

	invoking_call <- sys.call()

	assert(
		all(sapply(colls, is_collection)), invoking_call,
		exclaim$must_be_recursive_of_collections(
			colls, profile_object(colls)) )

	as.list(do.call(c, colls))
}

#' @export

xJoin... <- function (...) {
	xJoin(list(...))
}
