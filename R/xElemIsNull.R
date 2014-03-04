
#' xElemIsNull
#'
#' Is an element of a collection null?
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
#'
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
	# collection any -> vector Boolean

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		# empty pairlist - a slighty odd corner case.
		logical(0)
	} else {
		res <- vector(mode = 'logical', length(coll))

		for (ith in seq_along(coll)) {
			res[ith] <- identical(coll[[ith]], Null)
		}
		res
	}
})

#' @rdname xElemIsNull
#' @export

xElemIsNull... <- function (...) {
	xElemIsNull(list(...))
}
