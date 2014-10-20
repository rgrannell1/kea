
#' xNotNa
#'
#' Is an value not na?
#'
#' @section Type Signature:
#'     any -> <logical>
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

xNotNa <- MakeFun(function (val)

	if (length(val) != 1)
		TRUE
	else
		!isTRUE(is_na(val))

)
