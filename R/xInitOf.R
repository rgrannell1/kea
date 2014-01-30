
#' xInitOf
#'
#' Remove the last element of a collection.
#'
#' @param
#'    coll a collection. A collection to subset.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#'
#' @example
#'    inst/examples/example-xInitOf.R
#'
#' @rdname xInitOf
#' @export

xInitOf <- function (coll) {
	# Collection any -> [any]
	# return everything but the first element of a
	# collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0 || length(coll) == 1) {
		list()
	} else {
		coll <- as.list(coll)
		coll[-length(coll)]
	}
}

#' @rdname xInitOf
#' @export

xInitOf... <- function (...) {
	xInitOf(list(...))
}
