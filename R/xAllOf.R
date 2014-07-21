
#' xAllOf
#'
#' Is a predicate true for all elements of a collection?
#'
#' @section Type Signature:
#'     (any -> logical) -> |any| -> &lt;boolean>
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
#'    If coll is length zero then logical(0) is returned. Na values
#'    are treated as False.
#'
#' @family quantifier_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xAllOf.R
#'
#' @rdname xAllOf
#' @export

xAllOf <- MakeFun('xAllOf', function (pred, coll) {

	MACRO( Must_Have_Arity(pred, 1) )

	if (length(coll) == 0) {
		logical(0)
	} else {
		all(vapply(coll, function (elem) {

			is_match <- MACRO( Try_Higher_Order_Function(pred(elem)) )

			MACRO(Must_Be_Flag(is_match, pred))

			isTRUE(is_match)

		}, logical(1), USE.NAMES = False))
	}
})

#' @rdname xAllOf
#' @export

xAllOf_ <- MakeVariadic(xAllOf, 'coll')
