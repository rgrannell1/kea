
#' xNotNan
#'
#' Is an value not nan?
#'
#' @param
#'    val an arbitrary value.The value to test for
#'    not-nan.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xNotNan.R
#'
#' @section Corner Cases:
#'     xNotNan returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     True is returned.
#'
#' @family value_testing_functions
#'
#' @rdname xNotNan
#' @export

xNotNan <- MakeFun(function (val) {
	# any -> boolean
	# test if a value is na

	MACRO( Must $ Not_Be_Missing(val) )

	if (length(val) == 0) {
		True
	} else {
		!isTRUE(is.nan(val))
	}
})
