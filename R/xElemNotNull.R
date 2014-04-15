
#' xElemNotNull
#'
#' Is an element of a collection null?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being non-null.
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
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemNotNull.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemNotNull
#' @export

xElemNotNull <- MakeFun(function (coll) {
	# collection any -> vector Boolean
	# are the elements of a collection not null?

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		# empty pairlist - an odd corner case.
		logical(0)
	} else {
		res <- vector(mode = 'logical', length(coll))

		for (ith in seq_along(coll)) {
			res[ith] <- !identical(coll[[ith]], Null)
		}
		res
	}
})

#' @rdname xElemNotNull
#' @export

xElemNotNull... <- function (...) {
	xElemNotNull(list(...))
}
