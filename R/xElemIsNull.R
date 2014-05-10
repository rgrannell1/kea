
#' xElemIsNull
#'
#' Is an element of a collection null?
#'
#' @section Type Signature:
#'     |any| -> &lt;logical>
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being null.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns logical(0) if coll is itself Null.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemIsNull.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemIsNull
#' @export

xElemIsNull <- MakeFun(function (coll) {

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		# empty pairlist - a slighty odd corner case.
		logical(0)
	} else {
		vapply(coll, function (elem) {
			isTRUE(is.null(elem))
		}, logical(1))
	}
})

#' @rdname xElemIsNull
#' @export

xElemIsNull_ <- MakeVariadic(xElemIsNull, 'coll')
