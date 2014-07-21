
#' xLocate
#'
#' Get the indices of the elements that return true for a predicate.
#'
#' @section Type Signature:
#'     (any -> &lt;logical>) -> |any| -> &lt;integer>
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
#'      returns integer(0) if no match is found, or \bold{coll} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xLocate.R
#'
#' @rdname xLocate
#' @export

xLocate <- MakeFun('xLocate', function (pred, coll) {

	MACRO( Must_Have_Arity(pred, 1) )

	if (length(coll) == 0) {
		integer(0)
	} else {
		which( MACRO( Try_Higher_Order_Function( vapply(coll, function (x) {
			isTRUE(pred(x))
		}, logical(1), USE.NAMES = False) ) ) )
	}
})

#' @rdname xLocate
#' @export

xLocate_ <- MakeVariadic(xLocate, 'coll')
