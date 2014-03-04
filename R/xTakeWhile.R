
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

xTakeWhile <- MakeFun(function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# take every element in a collection
	# until a predicate returns false.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(pred) )
	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Fn_Matchable(pred) )
	MACRO( arrow ::: Must $ Be_Collection(coll) )

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		list()
	} else {

		try_hof({

			for (ith in seq_along(coll)) {

				is_match <- pred( coll[[ith]] )

				MACRO( arrow ::: Must $ Be_Flag(is_match, pred) )

				if (!isTRUE(is_match)) {
					return ( as.list(head(coll, ith - 1)) )
				}
			}},
			invoking_call
		)

		as.list(coll)
	}
})

#' @rdname xTakeWhile
#' @export

xTakeWhile... <- function (pred, ...) {
	xTakeWhile(pred, list(...))
}
