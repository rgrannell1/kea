
#' xInter
#'
#' Get the set intersection of several collections.
#'
#' @param
#'    colls a collection of collections.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero.
#'
#' @family
#'    collection_functions
#'
#' @export

xInter <- function (colls) {
	# Collection any -> Collection any -> Collection any
	# get the set intersection of two collections.

	invoking_call <- sys.call()

	colls <- lapply(colls, dearrowise)

	assert(
		all( sapply(colls, function (coll) {
			is_collection(coll)
		}) ), invoking_call,
		exclaim$must_be_collection_of_length(colls))

	if (length(colls) == 0) {
		list()
	} else if (length(colls) == 1) {
		colls[[1]]
	} else {

		colls <- do.call(c, colls)
		colls[!duplicated(colls)]
	}
}

#' @export

xInter... <- function (...) {
	xInter(list(...))
}
