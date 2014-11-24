
#' xIntersect
#'
#' Get the elements of a collection also in another.
#'
#' @section Type Signature:
#'     |any| -> |any| -> [any]
#'
#' @param
#'    coll1 a collection. The collection to test for membership in.
#'
#' @param
#'    coll2 a collection. The collection to return elements from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll1} or \bold{coll2} is length-zero. \bold{xIntersect}
#'    preserves the names of \bold{coll2} where possible.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xIntersect.R
#'
#' @rdname xIntersect
#' @export

xIntersect <- MakeFun(function (coll1, coll2)
	cIntersect(coll1, coll2)
)

#' @rdname xIntersect
#' @export

xIntersect_ <- MakeVariadic(xIntersect, 'coll2')
