
#' xIsEmpty
#'
#' Is a collection length-zero?
#'
#' @section Type Signature:
#'     |any| -> &lt;logical>
#'
#' @param
#'    coll a collection. The collection to test for being length zero.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    Returns false for non-empty collections.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xIsEmpty.R
#'
#' @family value_testing_functions
#'
#' @rdname xIsEmpty
#' @export

xIsEmpty <- MakeFun('xIsEmpty', function (coll) {
	isTRUE(length(coll) == 0)
})

#' @rdname xIsEmpty
#' @export

xIsEmpty_ <- MakeVariadic(xIsEmpty, 'coll')
