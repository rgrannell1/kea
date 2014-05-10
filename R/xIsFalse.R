
#' xIsFalse
#'
#' Is an value false?
#'
#' @section Type Signature:
#'     any -> &lt;logical>
#'
#' @param
#'    val an arbitrary value. The value to test for
#'    being false.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A true or false value.
#'
#' @example
#'    inst/examples/example-xIsFalse.R
#'
#' @section Corner Cases:
#'     xIsFalse returns either true or false, to make it
#'     safe for use with if statements. If val is length zero
#'     False is returned.
#'
#' @family value_testing_functions
#'
#' @rdname xIsFalse
#' @export

xIsFalse <- MakeFun(function (val) {

	MACRO( Must $ Not_Be_Missing(val) )

	isTRUE(identical(val, False))
})
