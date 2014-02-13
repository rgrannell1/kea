
#' xSecondOf
#'
#' Return the second element in a collection.
#'
#' @param
#'    coll a collection. The collection to get the
#'    second element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    The second element in \bold{coll}.
#'
#' @section Corner Cases:
#'    Throws an error if \bold{coll} has less than two
#'    elements; this is because any other corner case
#'    would violate the functions type-signature.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSecondOf.R
#'
#' @rdname xSecondOf
#' @export

xSecondOf <- function (coll) {
	# Collection any -> any
	# return the second element of a collection x.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(coll, invoking_call)
	insist $ must_be_longer_than(coll, 2, invoking_call)

	coll[[2]]
}

#' @rdname xSecondOf
#' @export

xSecondOf... <- function (...) {
	xSecondOf(list(...))
}
