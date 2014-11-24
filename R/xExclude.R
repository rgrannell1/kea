
#' xExclude
#'
#' Get the elements of a collection not in another.
#'
#' @section Type Signature:
#'     |any| -> |any| -> [any]
#'
#' @param
#'    coll1 a collection. The elements to exclude.
#'
#' @param
#'    coll2 a collection. The collection to exclude those elements from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll2} is length-zero.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xExclude.R

xExclude <- MakeFun(function (coll1, coll2)
	cExclude(coll1, coll2)
)

#' @rdname xExclude
#' @export

xExclude_ <- MakeVariadic(xExclude, 'coll2')
