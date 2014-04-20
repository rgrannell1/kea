
#' xNotFalse
#'
#' Is an value not false?
#'
#' @param
#'    val an arbitrary value.The value to test for
#'    not-falsity.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xNotFalse.R
#'
#' @section Corner Cases:
#'     xNotFalse returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     True is returned.
#'
#' @family value_testing_functions
#'
#' @rdname xNotFalse
#' @export

xNotFalse <- MakeFun(function (val) {
	# any -> boolean
	# test if a value is na

	MACRO( Must $ Not_Be_Missing(val) )

	isTRUE(identical(val, False))
})
