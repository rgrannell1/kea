
#' xJoin
#'
#' Concatenate several collections into one collection.
#'
#' @param
#'    colls several collections.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Empty collections act as a unit for concatenation;
#'    concatenating the empty list to another list returns
#'    the second list, without modification.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xJoin
#' @export

xJoin <- function (colls) {
	# Collection any coll -> [any]
	# Concatenate several collections
	# into one list.

	invoking_call <- sys.call()

	assert(
		all( vapply(colls, is_collection, logical(1)) ), invoking_call,
		exclaim$must_be_recursive_of_collections(
			colls, summate(colls)) )

	as.list(do.call(c, colls))
}

#' @rdname xJoin
#' @export

xJoin... <- function (...) {
	xJoin(list(...))
}
