
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

xAllOf <- MakeFun(function (pred, coll) {

	MACRO( Fix(xAllOf, pred, coll) )

	MACRO( Must $ Be_Fn_Matchable(pred) )
	MACRO( Must $ Be_Collection(coll) )

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		logical(0)
	} else {
		all(vapply(coll, function (elem) {
			isTRUE(pred(elem))
		}, logical(1), USE.NAMES = False))
	}
})

#' @rdname xAllOf
#' @export

xAllOf_ <- MakeVariadic(xAllOf, 'coll')
