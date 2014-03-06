
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

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		logical(0)
	} else {
		res <- vector(mode = 'logical', length(coll))

		for (ith in seq_along(coll)) {
			res[ith] <- identical(coll[[ith]], NaN)
		}

		res
	}
})

#' @rdname xElemIsNan
#' @export

xElemIsNan... <- function (...) {
	xElemIsNan(list(...))
}
