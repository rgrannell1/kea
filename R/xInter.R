
#' xInter
#'
#' Get the set intersection of several collections.
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
#'    Returns the empty list if \code{coll} is length-zero.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xInter
#' @export

xInter <- function (colls) {
	# Collection any -> Collection any -> Collection any
	# get the set intersection of two collections.

	invoking_call <- sys.call()

	insist$must_be_collection_of_collections(colls, invoking_call)

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

#' @rdname xInter
#' @export

xInter... <- function (...) {
	xInter(list(...))
}
