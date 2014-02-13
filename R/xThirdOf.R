
#' xThirdOf
#'
#' Return the third value in a collection.
#'
#' @param
#'    coll a collection. The collection to get the
#'    third element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    The third element in \bold{coll}.
#'
#' @section Corner Cases:
#'    Throws an error if \bold{coll} has less than
#'    three elements; this is because any other corner
#'    case would violate the function's type-signature.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xThirdOf.R
#'
#' @rdname xThirdOf
#' @export

xThirdOf <- function (coll) {
	# Collection any -> any
	# return the third element of a collection x.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(coll, invoking_call)
	insist $ must_be_longer_than(coll, 3, invoking_call)

	coll[[3]]
}

#' @rdname xThirdOf
#' @export

xThirdOf... <- function (...) {
	xThirdOf(list(...))
}
