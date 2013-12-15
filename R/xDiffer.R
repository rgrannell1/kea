
#' xDiffer
#'
#' Get the asymettric set difference of several collections.
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

xDiffer <- function (colls) {
	# Collection Collection any -> Collection any
	# get

	invoking_call <- sys.call()

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
		init <- list()

		for (ith in seq_along(colls)) {
			init <- setdiff( init, colls[[ith]] )
		}

		init
	}
}

#' @export

xDiffer... <- function (...) {
	xDiffer(list(...))
}
