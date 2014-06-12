
#' xMaxBy
#'
#' Get the largest value in a collection according to a measure function.
#'
#' @section Type Signature:
#'     (any -> &lt;number>) -> |any| -> any
#'
#' @param
#'     fn a unary function. The function to measure the size of
#'     an element.
#'
#' @param
#'    coll a non-empty collection. The collection to find the
#'    largest value of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An element from \bold{coll}.
#'
#' @section Corner Cases:
#'    If an empty collection is given, an error is thrown. This is because
#'    is impossible to return an element from a list that has none.
#'
#' @template
#'    Variadic
#'
#' @family selection_functions
#'
#' @example
#'    inst/examples/example-xMaxBy.R
#'
#' @rdname xMaxBy
#' @export

xMaxBy <- MakeFun(function (fn, coll) {

	MACRO( Must $ Be_Longer_Than(0, coll) )

	if (length(coll) == 1) {
		coll[[1]]
	} else {
		coll[[ which.max( vapply(coll, fn, numeric(1)) ) ]]
	}
})

#' @rdname xMaxBy
#' @export

xMaxBy_ <- MakeVariadic(xMaxBy, 'coll')
