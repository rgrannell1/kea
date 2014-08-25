
#' xSumBy
#'
#' Get the sum of a collection according to a measure function.
#'
#' @section Type Signature:
#'     (any -> &lt;number>) -> |any| -> &lt;number>
#'
#' @param
#'     fn a unary function. The function to measure the size of
#'     an element.
#'
#' @param
#'    coll a non-empty collection. The collection to find the
#'    sum of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A double.
#'
#' @section Corner Cases:
#'    If an empty collection is given, numeric(0) is returned.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSumBy.R
#'
#' @family math_functions
#'
#' @rdname xSumBy
#' @export

xSumBy <- MakeFun('xSumBy', function (fn, coll) {

	MACRO( Must_Have_Arity(fn, 1) )

	if (length(coll) == 0) {
		numeric(0)
	} else {
		`fn(coll)` <- MACRO(Try_Higher_Order_Function(
			vapply(coll, fn, numeric(1))
		))

		MACRO(Must_Be_Orderable(`fn(coll)`))

		sum(`fn(coll)`)
	}
})

#' @rdname xSumBy
#' @export

xSumBy_ <- MakeVariadic(xSumBy, 'coll')
