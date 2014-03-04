
#' xNotNa
#'
#' Is an value not na?
#'
#' @param
#'    val an arbitrary value.The value to test for
#'    not-na.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xNotNa.R
#'
#' @section Corner Cases:
#'     xNotNa returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     True is returned.
#'
#' @family value_testing_functions
#'
#' @rdname xNotNa
#' @export

xNotNa <- MakeFun(function (val) {
	# any -> boolean
	# test if a value is na

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(val) )

	if (length(val) == 0) {
		True
	} else {
		is.na(val)
	}
})
