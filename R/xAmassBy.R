
#' xAmassBy
#'
#' Group elements of a collection by the output of a function applied to each element,
#'     without keeping the result of the grouping function.
#
#' @section Type Signature:
#'     (any -> any) -> |any| -> [[any]]
#'
#' @details
#'     xAmassBy allows the grouping of a heterogeneous dataset into groups of elements
#'     similar in some way, without keeping the result of the grouping function like xGroupBy would.
#'
#' @param
#'     fn a unary function. The function returning the value to group its input by.
#'
#' @param
#'     coll a collection. The elements to group by a property.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list
#'
#' @section Corner Cases:
#'    If \bold{coll} is a empty collection the empty list is returned.
#'
#' @template
#'    C++
#'
#' @template
#'    Variadic
#'
#' @family reshaping_functions
#'
#' @example
#'    inst/examples/example-xAmassBy.R
#'
#' @rdname xAmassBy
#' @export

xAmassBy <- MakeFun(function (fn, coll) {

	MACRO( Must_Have_Arity(fn, 1) )

	MACRO(Try_Higher_Order_Function(
		cAmassBy(fn, coll)
	))

})

#' @rdname xAmassBy
#' @export

xAmassBy_ <- MakeVariadic(xAmassBy, 'coll')
