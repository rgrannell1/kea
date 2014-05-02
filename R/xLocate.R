
#' xLocate
#'
#' Get the indices of the elements that return true for a predicate.
#'
#' @section Type Signature:
#'     (any -> <logical>) -> |any| -> <integer>
#'
#' @param
#'    pred a predicate function. The function to test each element
#'    of a collection with.
#'
#' @param
#'    coll a collection. The collection with elements to test.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An length-one or length-zero whole number.
#'
#' @section Corner Cases:
#'      returns integer(0) if no match is found.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xLocate.R
#'
#' @rdname xLocate
#' @export

xLocate <- MakeFun(function (pred, coll) {

	MACRO( Must $ Not_Be_Missing(pred) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(pred) )
	MACRO( Must $ Be_Collection(coll) )

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		integer(0)
	} else {
		which( vapply(coll, function (x) {
			isTRUE(pred(x))
		}, logical(1), USE.NAMES = False) )
	}
})

#' @rdname xLocate
#' @export

xLocate_ <- MakeVariadic(xLocate, 'coll')
