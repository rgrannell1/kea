
#' xRestOf
#'
#' Remove the first element of a collection.
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
#' @example
#'    inst/examples/example-xRestOf.R
#'
#' @rdname xRestOf
#' @export

xRestOf <- function (coll) {
	# Collection a -> [a]
	# return everything but the first element of a
	# collection x.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) < 2) {
		list()
	} else {
		as.list( coll[-1] )
	}
}

#' @rdname xRestOf
#' @export

xRestOf... <- function (...) {
	xRestOf(list(...))
}
