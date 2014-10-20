
#' xIntersect
#'
#' Get the set intersection of two collections.
#'
#' @details
#'     The intersection of two sets is the collection of elements shared by both.
#'
#' @section Type Signature:
#'     |any| -> [any]
#'
#' @param
#'    coll1 a collection. The first collection to get the intersection of.
#'
#' @param
#'    coll1 a collection. The second collection to get the intersection of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
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
