
#' xUnion
#'
#' Get the set union of several collections.
#'
#' @details
#'    \code{xUnion} returns a list of the unique
#'    elements in the combination of each collection
#'    in \code{colls}.
#'
#' @param
#'    colls a collection of collections.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{colls} is length-zero.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xUnion
#' @export

xUnion <- function (colls) {
	# Collection any -> Collection any -> Collection any
	# get the set union of several collections.

	invoking_call <- sys.call()

	assert(
		all( vapply(colls, is_collection, logical(1)) ), invoking_call,
		exclaim$must_be_recursive_of_collections(
			colls, summate(colls)) )

	if (length(colls) == 0) {
		list()
	} else {
		unique(do.call(c, colls))
	}
}

#' @rdname xUnion
#' @export

xUnion... <- function (...) {
	xUnion(list(...))
}
