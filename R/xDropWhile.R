
#' xDropWhile
#'
#' Take every element in a collection from the first time a predicate
#' is false or na until the end of the collection.
#'
#' @section Type Signature:
#'     (any -> &lt;logical>) -> |any| -> [any]
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

	if (length(coll) == 0) {
		list()
	} else {

		for (ith in seq_along(coll)) {

			is_match <- pred( coll[[ith]] )

			MACRO( Must $ Be_Flag(is_match, pred) )

			if (!isTRUE(is_match)) {
				return (as.list( tail(coll, length(coll) - (ith - 1)) ))
			}
		}

		list()
	}
})

#' @rdname xDropWhile
#' @export

xDropWhile_ <- MakeVariadic(xDropWhile, 'coll')
