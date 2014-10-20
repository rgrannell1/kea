
#' xAllOf
#'
#' Is a predicate true for all elements of a collection?
#'
#' @section Type Signature:
#'     (any -> logical) -> |any| -> <boolean>
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

xAllOf <- MakeFun(function (pred, coll) {
	MACRO( Must_Have_Arity(pred, 1) )

	if (length(coll) == 0)
		logical(0)
	else

		MACRO( Try_Higher_Order_Function(

			all(vapply(coll, function (elem) {

				is_match <- pred(elem)

				MACRO(Must_Be_Flag(is_match, pred))

				identical(is_match, TRUE)

			}, logical(1), USE.NAMES = FALSE))

		) )

})

#' @rdname xAllOf
#' @export

xAllOf_ <- MakeVariadic(xAllOf, 'coll')
