
#' xIsInfixOf
#'
#' Is a collection an unbroken subsequence of another?
#'
#' @section Type Signature:
#'     |any| -> |any| -> &lt;logical>
#'
#' @param
#'    coll1 a collection. Then unbroken subsequence to test for.
#'
#' @param
#'    coll2 a collection. The collection to test for a subsequencematch.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If the first collection is longer than the second, False is returned.
#'    If either collection is length-zero, \bold{logical(0)} is returned.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xIsInfixOf.R
#'
#' @family value_testing_functions
#'
#' @rdname xIsInfixOf
#' @export

xIsInfixOf <- MakeFun(function (coll1, coll2) {
	cIsInfixOf(coll1, coll2)
})

#' @rdname xIsInfixOf
#' @export

xIsInfixOf_ <- MakeVariadic(xIsInfixOf, 'coll2')
