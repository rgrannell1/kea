
#' xMaxBy
#'
#' Get the largest value in a collection according to a measure function.
#'
#' @section Type Signature:
#'     (any -> <number>) -> |any| -> any
#'
#' @param
#'     fn a unary function. The function to measure the size of
#'     an element.
#'
#' @param
#'    coll a non-empty collection. The collection to find the
#'    largest value of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An element from \bold{coll}.
#'
#' @section Corner Cases:
#'    If an empty collection is given, an error is thrown. This is because
#'    is impossible to return an element from a list that has none.
#'
#' @template
#'    Variadic
#'
#' @family selection_functions
#' @family math_functions
#'
#' @example
#'    inst/examples/example-xMaxBy.R
#'
#' @rdname xMaxBy
#' @export

xMaxBy <- MakeFun(function (fn, coll) {

	MACRO( Must_Have_Arity(fn, 1) )

	MACRO( Must_Be_Longer_Than(0, coll) )

	if (length(coll) == 1)
		coll[[1]]
	else {

		`fn(coll)` <- MACRO(Try_Higher_Order_Function(
			vapply(coll, fn, numeric(1))
		))

		MACRO(Must_Be_Orderable(`fn(coll)`))

		coll[[ which.max(`fn(coll)`) ]]
	}
})

#' @rdname xMaxBy
#' @export

xMaxBy_ <- MakeVariadic(xMaxBy, 'coll')
