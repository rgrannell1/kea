
#' xUnion
#'
#' Get the set union of several collections.
#'
#' @param
#'    colls a collection of collections.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{colls} is length-zero.
#'
#' @family
#'    collection_functions
#'
#' @export

xUnion <- function (colls) {
	# Collection any -> Collection any -> Collection any
	# get the set union of several collections.

	invoking_call <- sys.call()

	colls <- lapply(colls, dearrowise)

	assert(
		all( sapply(colls, function (coll) {
			is_collection(coll)
		}) ), invoking_call,
		exclaim$must_be_collection_of_length(colls))

	if (length(colls) == 0) {
		list()
	} else {
		unique(do.call(c, colls))
	}
}

#' @export

xUnion... <- function (...) {
	xUnion(list(...))
}
