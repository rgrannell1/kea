
#' xIsNull
#'
#' Is an value null?
#'
#' @param
#'    val an arbitrary value. The value to test for
#'    being null.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xIsNull.R
#'
#' @section Corner Cases:
#'     xIsNull returns either true or false, to make it
#'     safe for use with if statements.
#'
#' @family value_testing_functions
#'
#' @rdname xIsNull
#' @export

xIsNull <- MakeFun(function (val) {
	# any -> boolean
	# test if a value is na

	MACRO( Must $ Not_Be_Missing(val) )

	is.null(val)
})
