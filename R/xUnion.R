
#' xUnion
#'
#' Get the set union of several collections.
#'
#' @param
#'    colls a collection of collections.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{colls} is length-zero.
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
		all( sapply(colls, is_collection) ), invoking_call,
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
