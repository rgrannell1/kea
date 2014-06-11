
#' xAnyOf
#'
#' Is a predicate true for any element of a collection?
#'
#' @section Type Signature:
#'     (any -> &lt;logical>) -> |any| -> &lt;logical>
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
#'    If coll is length zero then logical(0) is returned. Na values are treated as False.
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

	MACRO( Must $ Be_Collection(coll) )

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		logical(0)
	} else {

		for (elem in coll) {
			if ( isTRUE(pred(elem)) ) {
				return(True)
			}
		}

		False
	}
})

#' @rdname xAnyOf
#' @export

xAnyOf_ <- MakeVariadic(xAnyOf, 'coll')
