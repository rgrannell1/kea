
#' xDuplicatesOf
#'
#' Return the duplicated elements in a collection.
#'
#' @param
#'    coll a collection.
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
#'
#' @example
#'    inst/examples/example-xDuplicatesOf.R
#'
#' @rdname xDuplicatesOf
#' @export

xDuplicatesOf <- function (coll) {
	# Collection any -> Collection any
	# remove duplicated valeus from a collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		as.list(coll[ duplicated(coll) ])
	}
}

#' @rdname xDuplicatesOf
#' @export

xDuplicatesOf... <- function (...) {
	xDuplicatesOf(list(...))
}
