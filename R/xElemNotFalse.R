
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
	# Collection a -> Vector boolean
	# Is an element of a collection not false?

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (x) {
			!identical(x, False)
		}, logical(1), USE.NAMES = False)
	}
})

#' @rdname xElemNotFalse
#' @export

xElemNotFalse... <- function (...) {
	xElemNotFalse(list(...))
}
