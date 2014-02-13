
#' xTakeWhile
#'
#' Take every element in a collection from the start
#' until a predicate returns false.
#'
#' @param
#'    pred a predicate. The function to test each element of
#'    the collection with.
#'
#' @param
#'    coll a collection. The collection to drop elements from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is
#'    length-zero or the first element of
#'    \bold{coll} returns false for the predicate.
#'    Na values are considered false.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xTakeWhile.R
#'
#' @rdname xTakeWhile
#' @export

xTakeWhile <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# take every element in a collection
	# until a predicate returns false.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(pred)

	insist $ must_not_be_missing(coll)

	insist $ must_be_fn_matchable(pred, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		list()
	} else {

		try_hof({

			for (ith in seq_along(coll)) {

				is_match <- pred( coll[[ith]] )

				insist $ must_be_logical_result(is_match, pred, invoking_call)

				if (!isTRUE(is_match)) {
					return ( as.list(head(coll, ith - 1)) )
				}
			}},
			invoking_call
		)

		as.list(coll)
	}
}

#' @rdname xTakeWhile
#' @export

xTakeWhile... <- function (pred, ...) {
	xTakeWhile(pred, list(...))
}
