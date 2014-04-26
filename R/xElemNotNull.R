
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

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		# empty pairlist an odd corner case.
		logical(0)
	} else {

		vapply(coll, function (elem) {
			!isTRUE(is.null(elem))
		}, logical(1))
	}
})

#' @rdname xElemNotNull
#' @export

xElemNotNull_ <- MakeFun(function (...) {

	MACRO( Must $ Have_Canonical_Arguments() )

	xElemNotNull(list(...))
})
