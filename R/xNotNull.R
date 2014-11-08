
#' xNotNull
#'
#' Is an value not null?
#'
#' @section Type Signature:
#'     any -> <logical>
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
#' @section Corner Cases:
#'     xNotNull returns either true or false, to make it
#'     safe for use with if statements.
#'
#' @example
#'    inst/examples/example-xNotNull.R
#'
#' @family value_testing_functions
#'
#' @rdname xNotNull
#' @export

xNotNull <- MakeFun(function (val)
	!identical(val, NULL)
)
