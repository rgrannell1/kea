
#' xElemNotNan
#'
#' Is an element of a collection not NaN?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being non-nan.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{coll is length-zero}.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemNotNan.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemNotNan
#' @export

xElemNotNan <- MakeFun(function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection not false?

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (elem) {

			# atomic check required.
			!is.atomic(elem) || !isTRUE(is.nan(elem))

		}, logical(1))
	}
})

#' @rdname xElemNotNan
#' @export

xElemNotNan... <- function (...) {
	xElemNotNan(list(...))
}
