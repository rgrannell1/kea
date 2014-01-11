
#' xDiffer
#'
#' Get the asymettric set difference of several collections.
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
#' @rdname xDiffer
#' @export

xDiffer <- function (colls) {
	# Collection Collection any -> Collection any
	# get

	invoking_call <- sys.call()

	assert(
		all( vapply(colls, is_collection, logical(1)) ), invoking_call,
		exclaim$must_be_collection_of_length(
			colls, summate(colls)) )

	if (length(colls) == 0) {
		list()
	} else if (length(colls) == 1) {
		colls[[1]]
	} else {
		val <- list()

		for (ith in seq_along(colls)) {
			val <- setdiff( val, colls[[ith]] )
		}

		val
	}
}

#' @rdname xDiffer
#' @export

xDiffer... <- function (...) {
	xDiffer(list(...))
}
