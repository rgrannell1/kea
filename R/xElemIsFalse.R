
#' xElemIsFalse
#'
#' Is an element of a collection false?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being false.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemIsFalse.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemIsFalse
#' @export

xElemIsFalse <- MakeFun(function (coll) {
	# Collection a -> Vector boolean
	# Is an element of a collection false?

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	vapply(coll, function (elem) {
		!isTRUE(elem)
	}, logical(1))
})

#' @rdname xElemIsFalse
#' @export

xElemIsFalse_ <- function (...) {
	xElemIsFalse(list(...))
}
