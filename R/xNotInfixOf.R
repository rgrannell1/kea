
#' xNotInfixOf
#'
#' Is a collection not an unbroken subsequence of another?
#'
#' @section Type Signature:
#'     |any| -> |any| -> &lt;logical>
#'
#' @param
#'    coll1 a collection. Then unbroken subsequence to test for absense of.
#'
#' @param
#'    coll2 a collection. The collection to test for a subsequence match.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If the first collection is longer than the second, True is returned.
#'    If either collection is length-zero, \bold{logical(0)} is returned.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xNotInfixOf.R
#'
#' @family value_testing_functions
#'
#' @rdname xNotInfixOf
#' @export

xNotInfixOf <- MakeFun(function (coll1, coll2) {
	cNotInfixOf(coll1, coll2)
})

#' @rdname xNotInfixOf
#' @export

xNotInfixOf_ <- MakeVariadic(xNotInfixOf, 'coll2')
