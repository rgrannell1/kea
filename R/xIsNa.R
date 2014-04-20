
#' xIsNa
#'
#' Is an value na?
#'
#' @param
#'    val an arbitrary value. The value to test for being na.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xIsNa.R
#'
#' @section Corner Cases:
#'     xIsNa returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     False is returned.
#'
#' @family value_testing_functions
#'
#' @rdname xIsNa
#' @export

xIsNa <- MakeFun(function (val) {
	# any -> boolean
	# test if a value is na

	MACRO( Must $ Not_Be_Missing(val) )

	isTRUE(is.na(val))
})