
#' xLastOf
#'
#' Return the last element in a collection.
#'
#' @param
#'    coll a collection. The collection to return
#'    the last element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    The value of the last element in \bold{coll}.
#'
#' @section Corner Cases:
#'    Throws an error if \bold{coll} has less than one element; this is
#'    because any other corner case would violate the function's type-signature.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xLastOf.R
#'
#' @rdname xLastOf
#' @export

xLastOf <- function (coll) {
	# Collection any -> any
	# return the last element of a collection x,
	# using the subset operator

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist $ must_be_collection(coll, invoking_call)
	insist $ must_be_longer_than(coll, 0, invoking_call)

	coll[[ length(coll) ]]
}

#' @rdname xLastOf
#' @export

xLastOf... <- function (...) {
	xLastOf(list(...))
}
