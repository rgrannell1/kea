
#' xValuesOf
#'
#' Remove the names from a collection.
#'
#' @section Type Signature:
#'     |any| -> [any]
#'
#' @param
#'     coll a collection. The collection to remove names from.
#'
#' @param
#'     ... see above.
#'
#' @return
#'     a list.
#'
#' @section Corner Cases:
#'     Returns length-zero when \bold{coll} is length-zero.
#'
#' @family key_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xValuesOf.R
#'
#' @rdname xValuesOf
#' @export

xValuesOf <- MakeFun(function (coll) {
	as.list(unname(coll))
})

#' @rdname xValuesOf
#' @export

xValuesOf_ <- MakeVariadic(xValuesOf, 'coll')
