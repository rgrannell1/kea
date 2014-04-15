
#' xElemNotNa
#'
#' Test every element in a collection for being \code{Na} of any type.
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
	# Collection a -> Vector boolean
	# Is an element of a collection not na?

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		logical(0)
	} else {
		vapply(coll, function (x) {
			!identical(x, NA) &&
			!identical(x, NA_integer_) &&
			!identical(x, NA_real_) &&
			!identical(x, NA_character_) &&
			!identical(x, NA_complex_)
		}, logical(1), USE.NAMES = False)
	}
})

#' @rdname xElemNotNa
#' @export

xElemNotNa... <- function (...) {
	xElemNotNa(list(...))
}
