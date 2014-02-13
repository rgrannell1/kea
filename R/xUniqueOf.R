
#' xUniqueOf
#'
#' Return the unique elements in a collection.
#'
#' @param
#'    coll a collection. The collection to return unique
#'    elements from.
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
#' @example
#'    inst/examples/example-xUniqueOf.R
#'
#' @rdname xUniqueOf
#' @export

xUniqueOf <- function (coll) {
	# Collection any -> Collection any
	# remove duplicated values from a collection.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		as.list(unique(coll))
	}
}

#' @rdname xUniqueOf
#' @export

xUniqueOf... <- function (...) {
	xUniqueOf(list(...))
}
