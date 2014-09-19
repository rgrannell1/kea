
#' xIsSubsetOf
#'
#' Test if a collection is a subset of a second collection.
#'
#' @section Type Signature:
#'     |any| -> |any| -> &lt;logical>
#'
#' @param
#'    coll1 a collection. The set to test for membership in a superset.
#'
#' @param
#'    coll2 a collection. The superset to test.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    Returns logical(0) if \bold{coll1} or \bold{coll2} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xIsSubsetOf.R
#'
#' @family set_functions
#'
#' @rdname xIsSubsetOf
#' @export

xIsSubsetOf <- MakeFun('xIsSubsetOf', function (coll1, coll2) {
	cIsSubsetOf(coll1, coll2)
})

#' @rdname xIsSubsetOf
#' @export

xIsSubsetOf_ <- MakeVariadic(xIsSubsetOf, 'coll2')
