
#' xRiffle
#'
#' Insert an element in between each element of a list.
#'
#' @section Type Signatures:
#'     any -> |any| -> [any]
#'
#' @param val an arbitrary value. The value to intersperse
#'     between elements in the list.
#'
#' @param coll a collection. The collection to insert elements throughout.
#'
#' @param ... see above.
#'
#' @return A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is empty.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @template
#'    C++
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xRiffle.R
#'
#' @template
#'    S-Experimental
#'
#' @rdname xRiffle
#' @export

xRiffle <- MakeFun(function (val, coll) {
	cRiffle(val, coll)
})

#' @rdname xRiffle
#' @export

xRiffle_ <- MakeVariadic(xRiffle, 'coll')
