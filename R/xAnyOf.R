
#' xAnyOf
#'
#' Is a predicate true for any element of a collection?
#'
#' @section Type Signature:
#'     (any -> <logical>) -> |any| -> <logical>
#'
#' @param
#'    pred a predicate. The function used to test each element of
#'    the input collection.
#'
#' @param
#'    coll a collection. The collection to test each element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If coll is length zero then logical(0) is returned.
#'    Na values are treated as False.
#'
#' @family quantifier_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xAnyOf.R
#'
#' @rdname xAnyOf
#' @export

xAnyOf <- MakeFun(function (pred, coll) {

	MACRO( Must_Have_Arity(pred, 1) )

	if (length(coll) == 0)
		logical(0)
	else

		MACRO( Try_Higher_Order_Function({

			for (elem in coll) {

				is_match <- pred(elem)

				MACRO(Must_Be_Flag(is_match, pred))

				if (isTRUE(is_match)) {
					return (TRUE)
				}
			}

			FALSE
		}) )

})

#' @rdname xAnyOf
#' @export

xAnyOf_ <- MakeVariadic(xAnyOf, 'coll')
