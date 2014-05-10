
#' xElemNotTrue
#'
#' Is an element of a collection not true?
#'
#' @section Type Signature:
#'     |any| -> &lt;logical>
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being non-true.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns logical(0) if \bold{coll} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemNotTrue.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemNotTrue
#' @export

xElemNotTrue <- MakeFun(function (coll) {

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (elem) {
			!isTRUE(elem)
		}, logical(1))
	}
})

#' @rdname xElemNotTrue
#' @export

xElemNotTrue_ <- MakeVariadic(xElemNotTrue, 'coll')
