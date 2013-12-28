
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
#' @family collection_functions
#'
#' @family set_functions
#'
#' @export

xInter <- function (colls) {
	# Collection any -> Collection any -> Collection any
	# get the set intersection of two collections.

	invoking_call <- sys.call()

	assert(
		all( sapply(colls, is_collection(coll)) ),
		invoking_call,
		exclaim$must_be_collection_of_length(
			colls, profile_object(colls)) )

	if (length(colls) == 0) {
		list()
	} else if (length(colls) == 1) {
		colls[[1]]
	} else {

		overlap <- colls[[1]]

		for (ith in 2:length(colls)) {
			overlap <- intersect( overlap, colls[[ith]] )
		}

		as.list(overlap)
	}
}

#' @export

xInter... <- function (...) {
	xInter(list(...))
}
