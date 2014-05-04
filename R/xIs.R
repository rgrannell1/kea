
#' xIs
#'
#' Are two values equal?
#'
#' @section Type Signature:
#'     any -> any -> <logical>
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
#'    inst/examples/example-xIs.R
#'
#' @family value_testing_functions
#'
#' @section Corner Cases:
#'    \bold{xIs} does not allow for minor numeric inequality (0.999999 != 1),
#'    and treats -0. and 0. as identical.
#'
#' @rdname xIs
#' @export

xIs <- MakeFun(function (val1, val2) {

	MACRO( Must $ Not_Be_Missing(val1) )
	MACRO( Must $ Not_Be_Missing(val2) )

	# -- NaN == Nan, Na == Na, 0. == -0.
	identical(val1, val2)

})
