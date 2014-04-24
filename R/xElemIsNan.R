
#' xElemIsNan
#'
#' Is an element in a collection NaN?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being nan.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns True if coll is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemIsNan.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemIsNan
#' @export

xElemIsNan <- MakeFun(function (coll) {
	# collection any -> vector Boolean

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (elem) {
			# fails without is atomic.
			is.atomic(elem) && isTRUE(is.nan(elem))

		}, logical(1))
	}
})

#' @rdname xElemIsNan
#' @export

xElemIsNan_ <- function (...) {
	xElemIsNan(list(...))
}
