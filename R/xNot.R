
#' xNot
#'
#' Are two values not equal?
#'
#' @section Type Signature:
#'     any -> any -> &lt;logical>
#'
#' @param
#'    val1 an arbitrary value. The first value to test.
#'
#' @param
#'    val2 an arbitrary value. The second value to test.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xNot.R
#'
#' @family value_testing_functions
#'
#' @section Corner Cases:
#'    \bold{xNot} does not allow for minor numeric inequality (0.999999 != 1),
#'    and treats -0. and 0. as identical.
#'
#' @rdname xNot
#' @export

xNot <- MakeFun(function (val1, val2) {

	# -- NaN == Nan, Na == Na, 0. == -0.
	!identical(val1, val2)
})
