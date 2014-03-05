#
#' xNotNull
#'
#' Is an value not null?
#'
#' @param
#'    val an arbitrary value. The value to test for
#'    not being null.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xNotNull.R
#'
#' @section Corner Cases:
#'     xNotNull returns either true or false, to make it
#'     safe for use with if statements.
#'
#' @family value_testing_functions
#'
#' @rdname xNotNull
#' @export

xNotNull <- MakeFun(function (val) {
	# any -> boolean
	# test if a value is na

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(val) )

	!is.null(val)
})
