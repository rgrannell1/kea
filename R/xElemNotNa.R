
#' xElemNotNa
#'
#' Test every element in a collection for being \code{Na} of any type.
#'
#' @section Type Signature:
#'     |any| -> &lt;logical>
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being non-na.
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
#'    inst/examples/example-xElemNotNa.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemNotNa
#' @export

xElemNotNa <- MakeFun(function (coll) {

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (elem) {
			!isTRUE(is.na(elem))
		}, logical(1))
	}
})

#' @rdname xElemNotNa
#' @export

xElemNotNa_ <- MakeVariadic(xElemNotNa, 'coll')
