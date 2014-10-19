
#' xNotSubsetOf
#'
#' Test if a collection is not a subset of a second collection.
#'
#' @section Type Signature:
#'     |any| -> |any| -> <logical>
#'
#' @param
#'    coll1 a collection. The set to test for non-membership in a superset.
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
#'    C++
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xNotSubsetOf.R
#'
#' @family set_functions
#'
#' @rdname xNotSubsetOf
#' @export

xNotSubsetOf <- MakeFun(function (coll1, coll2)
	cNotSubsetOf(coll1, coll2)
)

#' @rdname xNotSubsetOf
#' @export

xNotSubsetOf_ <- MakeVariadic(xNotSubsetOf, 'coll2')
