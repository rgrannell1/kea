
#' xElemNotFalse
#'
#' Is an element of a collection not false?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being non-false.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemNotFalse.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemNotFalse
#' @export

xElemNotFalse <- MakeFun(function (coll) {

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

#' @rdname xElemNotFalse
#' @export

xElemNotFalse_ <- MakeFun(function (...) {

	MACRO( Must $ Have_Canonical_Arguments() )

	xElemNotFalse(list(...))
})
