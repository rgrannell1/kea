
#' xNotNan
#'
#' Is an value not nan?
#'
#' @section Type Signature:
#'     any -> <logical>
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

xNotNan <- MakeFun(function (val)
	!isTRUE(identical(val, NaN))
)
