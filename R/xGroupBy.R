
#' xGroupBy
#'
#' Group elements of a collection by the output of a function applied to each element.
#
#' @section Type Signature:
#'     (any -> any) -> |any| -> [[any]]
#'
#' @details
#'     xGroupBy allows the grouping of a heterogeneous dataset into groups of elements
#'     similar in some way.
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
#'    inst/examples/example-xGroupBy.R
#'
#' @rdname xGroupBy
#' @export

xGroupBy <- MakeFun('xGroupBy', function (fn, coll) {

	MACRO( Must_Have_Arity(fn, 1) )

	cGroupBy(fn, coll)
})

#' @rdname xGroupBy
#' @export

xGroupBy_ <- MakeVariadic(xGroupBy, 'coll')
