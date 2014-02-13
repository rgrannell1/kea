
#' xLocate
#'
#' Get the position of the first element for which a
#' predicate returns true.
#'
#' @param
#'    pred a predicate function. The function to test each element
#'    of a collection with.
#'
#' @param
#'    coll a collection. The collection with elements to test.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An length-one or length-zero whole number.
#'
#' @section Corner Cases:
#'      returns integer(0) if no match is found.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xLocate.R
#'
#' @rdname xLocate
#' @export

xLocate <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# returns the first index of collection that matches
	# the predicate pred.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(pred)

	insist $ must_not_be_missing(coll)

	insist $ must_be_fn_matchable(pred, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		integer(0)
	} else {

		try_hof({
			which( vapply(coll, pred, logical(1), USE.NAMES = False) )
			},
			invoking_call
		)
	}
}

#' @rdname xLocate
#' @export

xLocate... <- function (pred, ...) {
	xLocate(pred, list(...))
}
