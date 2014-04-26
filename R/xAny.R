
#' xAny
#'
#' Is a predicate true for any element of a collection?
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
#'    ... see above.MakeFun(
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If coll is length zero then logical(0) is returned.
#'
#' @family quantifier_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xAny.R
#'
#' @rdname xAny
#' @export

xAny <- MakeFun(function (pred, coll) {

	MACRO( Must $ Not_Be_Missing(pred) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(pred) )
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

#' @rdname xAny
#' @export

xAny_ <- MakeFun(function (pred, ...) {

	MACRO( Must $ Have_Canonical_Arguments() )

	xAny(pred, list(...))
})
