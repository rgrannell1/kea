
#' xOneOf
#'
#' Select a random value from a collection.
#'
#' @param
#'      coll a collection. The collection to take a value from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'      A value from \bold{coll}.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{coll} is length-zero.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xOneOf.R
#'
#' @rdname xOneOf
#' @export

xOneOf <- function (coll) {
	# collectionction any -> any

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)
	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else if (length(coll) == 1) {
		coll
	} else {
		sample(coll, size = 1)
	}
}

#' @rdname xOneOf
#' @export

xOneOf... <- function (...) {
	xOneOf(list(...))
}
