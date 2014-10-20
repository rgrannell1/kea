
#' xNoneOf
#'
#' Is a predicate true for none elements of a collection?
#'
#' @section Type Signature:
#'     (any -> <logical>) -> |any| -> <boolean>
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
#'    Na is treated as False.
#'
#' @family quantifier_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xNoneOf.R
#'
#' @rdname xNoneOf
#' @export

xNoneOf <- MakeFun(function (pred, coll)

	if (length(coll) == 0)
		logical(0)
	else

		MACRO( Try_Higher_Order_Function(
			!any(vapply(coll, function (elem) {

				is_match <- pred(elem)

				MACRO(Must_Be_Flag(is_match, pred))

				is_match

			}, logical(1), USE.NAMES = False))
		))

)

#' @rdname xNoneOf
#' @export

xNoneOf_ <- MakeVariadic(xNoneOf, 'coll')
