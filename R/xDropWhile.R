
#' xDropWhile
#'
#' Take every element in a collection from the first time a predicate
#' is false or na until the end of the collection.
#'
#' @param
#'    pred a predicate. The functionto test each element of
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
#'	  Returns the emty list if \bold{coll} is length-zero.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#'
#' @example
#'    inst/examples/example-xDropWhile.R
#'
#' @rdname xDropWhile
#' @export

xDropWhile <- MakeFun(function (pred, coll) {
	# (any -> logical) -> Collection any -> [any]
	# take every element from the first element for which
	# pred is false to the end of coll

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
					return (as.list( tail(coll, length(coll) - (ith - 1)) ))
				}
			}},
			invoking_call
		)

		list()
	}
})

#' @rdname xDropWhile
#' @export

xDropWhile... <- function (pred, ...) {
	xDropWhile(pred, list(...))
}
