
#' xIsNan
#'
#' Is an value nan?
#'
#' @section Type Signature:
#'     any -> &lt;logical>
#'
#' @param
#'    val an arbitrary value. The value to test for being nan.
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

xIsNan <- MakeFun('xIsNan', function (val) {
	isTRUE(identical(val, NaN))
})
