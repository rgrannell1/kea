
#' xIsNan
#'
#' Is an value nan?
#'
#' @param
#'    val an arbitrary value. The value to test for being
#'    nan.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xIsNan.R
#'
#' @section Corner Cases:
#'     xIsNan returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     False is returned.
#'
#' @family value_testing_functions
#'
#' @rdname xIsNan
#' @export

xIsNan <- MakeFun(function (val) {
	# any -> boolean
	# test if a value is na

	MACRO( Must $ Not_Be_Missing(val) )

	if (length(val) == 0) {
		False
	} else {
		is.nan(val)
	}
})
