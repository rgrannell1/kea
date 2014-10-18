
#' xLenOf
#'
#' Get the length of a collection.
#'
#' @section Type Signature:
#'     |any| -> <number>
#'
#' @param
#'    coll a collection. The collection to test the
#'    length of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A nonnegative integer.
#'
#' @section Corner Cases:
#'      Returns zero if \bold{coll} is empty.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xLenOf.R
#'
#' @family math_functions
#'
#' @rdname xLenOf
#' @export

xLenOf <- MakeFun(function (coll) {
	length(coll)
})

#' @rdname xLenOf
#' @export

xLenOf_ <- MakeVariadic(xLenOf, 'coll')
