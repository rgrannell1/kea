
#' xDropWhile
#'
#' Take every element in a collection from the first time a predicate
#' is false or na until the end of the collection.
#'
#' @section Type Signature:
#'     (any -> <logical>) -> |any| -> [any]
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
#'	  Returns the empty list if \bold{coll} is length-zero.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xDropWhile.R
#'
#' @rdname xDropWhile
#' @export

xDropWhile <- MakeFun(function (pred, coll) {

	MACRO( Must_Have_Arity(pred, 1) )

	if (length(coll) == 0)
		keep_names(list(), coll)
	else

		MACRO( Try_Higher_Order_Function({

			for ( ith in seq_len(length(coll)) ) {

				is_match <-  pred( coll[[ith]] )

				MACRO( Must_Be_Flag(is_match, pred) )

				if (!is_match) {
					return (as.list( tail(coll, length(coll) - (ith - 1)) ))
				}
			}

			keep_names(list(), coll)

		}) )

})

#' @rdname xDropWhile
#' @export

xDropWhile_ <- MakeVariadic(xDropWhile, 'coll')
