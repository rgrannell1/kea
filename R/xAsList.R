
#' xAsList
#'
#' Convert a collection to a list.
#'
#' @section Type Signature:
#'     |any| -> [any]
#'
#' @param
#'    coll a collection. The collection to convert to a list.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'     xAsList preserves the names of its input collection.
#'
#' @template
#'    Variadic
#'
#' @family container_conversion_functions
#'
#' @example
#'    inst/examples/example-xAsCharacter.R
#'
#' @rdname xAsList
#' @export

xAsList <- MakeFun('xAsList', function (coll) {
	as.list(coll)
})

#' @rdname  xAsList
#' @export

xAsList_ <- MakeVariadic(xAsList, 'coll')
